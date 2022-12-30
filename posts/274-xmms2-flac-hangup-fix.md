---
title: "xmms2 FLAC hangup fix"
date: December 30, 2022
---

Over past few months I noiced that `xmms2` started getting stuck on some
songs in my playlist. The typical symptom is lack of sound after the
song finished. There was no (expected!) switch to the next song. Instead
existing playlist entry went to out-of-duration playtime report:

```
$ xmms2 status
Playing: Artist - Song: 25:06 of 08:01
```

In this case `08:01` is the song length and `25:06` is the offset within
that song (should never be more than `08:01` in this case).

When `xmms2` was in this state CPU load was still low as if it played
song just fine. Manual attempts to seek within this song
(`$ xmms2 seek +10`) would instantly switch it to the next one as of
just finished.

I was not sure when exactly this effect started or what types of songs
it affects. Anecdotally it felt like 1-2% of songs were affected. And
they were always the same songs. That hinted at recent decoding changes
somewhere.

I looked up the shortest song that exhibited the problem. Best I could
find was a 3 minutes 57 seconds long sample. Not too long, but long
enough to avoid whole system bisection.

Instead I ran `./xmms2d --verbose` and started playing the sample
problematic. Once it was over 4 minutes `xmms2`'s debug logs started
growing rapidly with error like:

```
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER
...
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH
...
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER
...
11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH
```

That was a good enough hint that `FLAC` decoding state machine did not
feel well. Glancing at `xiph/flac` issue tracker I found seemingly
similar [Issue #487](https://github.com/xiph/flac/issues/487). There
someone found that `flac-1.4.0` release became more picky around stream
validation. The report also provides numeros commands to validate and
re-encode `.flac` files using `flac` command. Very handy!

Unfortunately my files all passed `flac -t` validation and did not
complain about internal inconsistencies. I means I I probably had a
different problem. But that gave me a hint that maybe it's related to
`flac` library update in my system. I downgraded `1.4.2` down to `1.3.4`
got my hangups disappeared!

`xmms2`'s internal architecture is simple: it handles data streams as
explicit objects by passing them through chain of plugins. Each plugin
reads data from previous chained plugin and writes data to next chained
plugin. The plugin itself can perform data transformation as well.

For example to playback a `.flac` file one needs a few `xmms2` plugins:

- `file` plugin to read files from disk)
- `flac` plugin to convert `FLAC` stream to simpler `PCM` stream
- `pulse` plugin to write `PCM` into a sound subsystem.

Such a stream plugin abstraction makes it trivial to explore ways
of individual file formats handling in `xmms2`. For example this
is our (yet unfixed) full
[flac plugin code](https://github.com/xmms2/xmms2-devel/blob/9bfbc687fc586c56443f9ea296988eefd759c30d/src/plugins/flac/flac.c).

A few things to note there:

- `xmms_flac_data_t` type defines our decoder context
- `xmms_flac_plugin_setup()` registers plugin to handle `audio/x-flac` files
- `methods.init = xmms_flac_init;` and `methods.read = xmms_flac_read;` define
  callbacks to implement `FLAC` stream decoding.

`xmms_flac_init()` attaches `FLAC` handler to a new stream. It creates
`FLAC` decoder and defines it's output stream type (like 16-bit `PCM`):

```c
static gboolean
xmms_flac_init (xmms_xform_t *xform)
{
    xmms_flac_data_t *data;
    data = g_new0 (xmms_flac_data_t, 1);
    data->flacdecoder = FLAC__stream_decoder_new ();
    init_status = FLAC__stream_decoder_init_stream (data->flacdecoder,
                                                    flac_callback_read,
                                                    flac_callback_seek,
                                                    flac_callback_tell,
                                                    flac_callback_length,
                                                    flac_callback_eof,
                                                    flac_callback_write,
                                                    flac_callback_metadata,
                                                    flac_callback_error,
                                                    xform);
    ...
    } else if (data->bits_per_sample == 16) {
        sample_fmt = XMMS_SAMPLE_FORMAT_S16;
    }
    ...

    xmms_xform_outdata_type_add (xform,
                                 XMMS_STREAM_TYPE_MIMETYPE,
                                 "audio/pcm",
                                 XMMS_STREAM_TYPE_FMT_FORMAT,
                                 sample_fmt,
                                 XMMS_STREAM_TYPE_FMT_CHANNELS,
                                 data->channels,
                                 XMMS_STREAM_TYPE_FMT_SAMPLERATE,
                                 data->sample_rate,
                                 XMMS_STREAM_TYPE_END);
    data->buffer = g_string_new (NULL);
    return TRUE;
```

Many lines of code, but it's just 2 function calls with many details
passed around as indivifual function aprameters. Incidenally `flac`
library itself has a similar API: we pass in a decoder context object
to `FLAC__stream_decoder_init_stream()` and a bunch of callback to read
input and write output.

In case of our error message spam it all came from this
`flac_callback_error()` definition:

```c
static void
flac_callback_error (const FLAC__StreamDecoder *flacdecoder,
                     FLAC__StreamDecoderErrorStatus status,
                     void *client_data)
{
    xmms_xform_t *data = (xmms_xform_t *) client_data;

    g_return_if_fail (flacdecoder);
    g_return_if_fail (data);

    XMMS_DBG ("%s", FLAC__StreamDecoderErrorStatusString[status]);
}
```

`11:45:35 DEBUG: ../src/plugins/flac/flac.c:341: FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC`
error messages we saw earlier originate here.

`flac` plugin tried to read data from seemingly broken (or finished?)
stream again and again. Here is the full `flac_callback_read` definition:

```c
static FLAC__StreamDecoderReadStatus
flac_callback_read (const FLAC__StreamDecoder *flacdecoder, FLAC__byte buffer[],
                    size_t *bytes, void *client_data)
{
    xmms_xform_t *xform = (xmms_xform_t *) client_data;
    xmms_error_t error;
    gint ret;

    g_return_val_if_fail (xform, FLAC__STREAM_DECODER_READ_STATUS_ABORT);

    ret = xmms_xform_read (xform, (gchar *)buffer, *bytes, &error);
    if (ret == 0) {
        return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
    } else if (ret < 0) {
        return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
    } else {
        *bytes = ret;
        return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
    }
}
```

Not getting too much into `FLAC` state machine can you spot anything
suspicious about this function?

The idea of this function is simple: read a few bytes from
`xmms_xform_t *` stream via `xmms_xform_read()` and write it into the
`buffer` byte array.

`buffer` output array is `*bytes` bytes long. How do we tell if we
managed to read less data than needed to `flac` library? Here is what
`FLAC/stream_decoder.h` [has to say](https://github.com/xiph/flac/blob/master/include/FLAC/stream_decoder.h#L475) about it:

```
/** Signature for the read callback.
 *
 *  A function pointer matching this signature must be passed to
 *  FLAC__stream_decoder_init*_stream(). The supplied function will be
 *  called when the decoder needs more input data.  The address of the
 *  buffer to be filled is supplied, along with the number of bytes the
 *  buffer can hold.  The callback may choose to supply less data and
 *  modify the byte count but must be careful not to overflow the buffer.
 *  The callback then returns a status code chosen from
 *  FLAC__StreamDecoderReadStatus.
 *
 * Here is an example of a read callback for stdio streams:
 * \code
 * FLAC__StreamDecoderReadStatus read_cb(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data)
 * {
 *   FILE *file = ((MyClientData*)client_data)->file;
 *   if(*bytes > 0) {
 *     *bytes = fread(buffer, sizeof(FLAC__byte), *bytes, file);
 *     if(ferror(file))
 *       return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
 *     else if(*bytes == 0)
 *       return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
 *     else
 *       return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
 *   }
 *   else
 *     return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
 * }
 * \endcode
 *
 * \note In general, FLAC__StreamDecoder functions which change the
 * state should not be called on the \a decoder while in the callback.
 *
 * \param  decoder  The decoder instance calling the callback.
 * \param  buffer   A pointer to a location for the callee to store
 *                  data to be decoded.
 * \param  bytes    A pointer to the size of the buffer.  On entry
 *                  to the callback, it contains the maximum number
 *                  of bytes that may be stored in \a buffer.  The
 *                  callee must set it to the actual number of bytes
 *                  stored (0 in case of error or end-of-stream) before
 *                  returning.
 * \param  client_data  The callee's client data set through
 *                      FLAC__stream_decoder_init_*().
 * \retval FLAC__StreamDecoderReadStatus
 *    The callee's return status.  Note that the callback should return
 *    \c FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM if and only if
 *    zero bytes were read and there is no more data to be read.
 */
typedef FLAC__StreamDecoderReadStatus (*FLAC__StreamDecoderReadCallback)(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data);
```

While a bit verbose the documentation string even has the example
implementation of a sinble callback almost identical to `xmms2`'s
use case.

Note: `*bytes` is unconditionally(ish) written back in the example
above. We always signal library what we passed back regardless of
encountered errors. It's because `flac` knows how to skip over
undecodable metadata. Thus it's a resonable behaviour to
return `FLAC__STREAM_DECODER_READ_STATUS_ABORT` and still expect more
reads from this stream in search of decodable next frame.

In comparison `xmms2` did not update `*bytes` in case of end of stream
and in case of an error. As a result `flac` decoder was stuck running
the callback again and again getting the same `buffer` back. `buffer`
possibly contained uninitialized contents of `buffer` as it if was just
read them from the input stream.

Once understood the fix was [trivial](https://github.com/xmms2/xmms2-devel/commit/39d31d4a7ae463f3df7a09915fe61e2574f4d95f):

```diff
--- a/src/plugins/flac/flac.c
+++ b/src/plugins/flac/flac.c
@@ -138,26 +138,28 @@ xmms_flac_plugin_setup (xmms_xform_plugin_t *xform_plugin)
 static FLAC__StreamDecoderReadStatus
 flac_callback_read (const FLAC__StreamDecoder *flacdecoder, FLAC__byte buffer[],
                     size_t *bytes, void *client_data)
 {
        xmms_xform_t *xform = (xmms_xform_t *) client_data;
        xmms_error_t error;
        gint ret;

        g_return_val_if_fail (xform, FLAC__STREAM_DECODER_READ_STATUS_ABORT);

        ret = xmms_xform_read (xform, (gchar *)buffer, *bytes, &error);
        if (ret == 0) {
+               *bytes = 0;
                return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
        } else if (ret < 0) {
+               *bytes = 0;
                return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
        } else {
                *bytes = ret;
                return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
        }
 }
```

After that I saw no hangups in `.flac` files so far.

## Parting words

`xmms2`'s plugins are usually very simple to read and implement.

In case of more complex problems `xmms2d --verbose` flag is useful to
enable `XMMS_DBG()` debugging output. If nothing else it should help
finding out exact plugins used to playback a problematic file.

`flac-1.4.0` subtly changed the recovery code around invalid streams and
managed to expose long standing bug in `xmms2` code base. Luckily it was
trivial to fix once identified.

Have fun!
