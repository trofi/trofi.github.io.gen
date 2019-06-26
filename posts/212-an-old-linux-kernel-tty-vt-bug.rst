---
title: "An old linux kernel tty/vt bug"
date: June 26, 2019
---

:PostID: 212
:Title: "An old linux kernel tty/vt bug"
:Keywords: linux, kernel, vt, intel, gentoo, framebuffer
:Categories: notes

This post is another one in series of obscure bugs. This time
elusive bug manifested on my desktop for years until it was pinned down
by luck.

The Bug
-------

Initial bug manifested in a very magical way: I boot up my desktop,
start a window manager, use it for a week and then at some point
when I press Ctrl-F1 my machine reboots gracefully. System logs
say I pressed **power** button. I did not though :)

That kept happening once in a few months and was very hard to say
what changed.

I was not sure how to debug that. My only clue was the following message
in boot logs:

.. code-block::

    Mar 29 19:22:42 sf systemd-logind[413]: Power key pressed
    <graceful shutdown goes here>

To workaround the effect I made poweroff a no-op in systemd. I hever use
"power" button.

.. code-block:: Diff

    --- a/src/login/logind-button.c
    +++ b/src/login/logind-button.c
    @@ -172,3 +172,3 @@ static int button_dispatch(sd_event_source *s, int fd, uint32_t revents, void *u
     
    -    manager_handle_action(b->manager, INHIBIT_HANDLE_POWER_KEY, b->manager->handle_power_key, b->manager->power_key_ignore_inhibited, true);
    +    //manager_handle_action(b->manager, INHIBIT_HANDLE_POWER_KEY, b->manager->handle_power_key, b->manager->power_key_ignore_inhibited, true);
         break;

The patch still kept messages popping up in the logs but did not shutdown
my machine any more. This allowed me to track frequency of these events
without distracting actual work on the machine.

But how one would find out how to track it down to a faulty component?
Was it my hardware (keyboard, USB host, etc.) losing mind for a second
or some obscure software bug?

I tried to track it down backwards from "Power key pressed" in systemd
down to a source that registered generated the event.

Apparently all systemd does is reading **/dev/input/event<N>** device
for **power** keypress and reacts accordingly. That means kernel itself
sends those signals as **code=KEY_POWER** and **code=KEY_POWER2** values
of **struct input_event**. I was not able to trace it down to my
keyboard driver at that time.

The clue
--------

A few years passed. I forgot about the local systemd patch.

And one day I got a very scary kernel backtraces when my system booted:

.. code-block::

    Apr 29 13:12:24 sf kernel: BUG: unable to handle kernel paging request at ffffa39b3b117000
    Apr 29 13:12:24 sf kernel: #PF error: [PROT] [WRITE]
    Apr 29 13:12:24 sf kernel: PGD 5e4a01067 P4D 5e4a01067 PUD 5e4a06067 PMD 7f7d0f063 PTE 80000007fb117161
    Apr 29 13:12:24 sf kernel: Oops: 0003 [#1] PREEMPT SMP
    Apr 29 13:12:24 sf kernel: CPU: 7 PID: 423 Comm: loadkeys Tainted: G         C        5.1.0-rc7 #98
    Apr 29 13:12:24 sf kernel: Hardware name: Gigabyte Technology Co., Ltd. To be filled by O.E.M./H77M-D3H, BIOS F12 11/14/2013
    Apr 29 13:12:24 sf kernel: RIP: 0010:__memmove+0x81/0x1a0
    Apr 29 13:12:24 sf kernel: Code: 4c 89 4f 10 4c 89 47 18 48 8d 7f 20 73 d4 48 83 c2 20 e9 a2 00 00 00 66 90 48 89 d1 4c 8b 5c 16 f8 4c 8d 54 17 f8 48 c1 e9 03 <f3> 48 a5 4d 89 1a e9 0c 01 00 00 0f 1f 40 00 48 89 d1 4c $
    Apr 29 13:12:24 sf kernel: RSP: 0018:ffffc0c3c0c7fd08 EFLAGS: 00010203
    Apr 29 13:12:24 sf kernel: RAX: ffffa39b39c9b08c RBX: 0000000000000019 RCX: 00000b8c90633fcb
    Apr 29 13:12:24 sf kernel: RDX: 00005c648461bdcd RSI: ffffa39b3b116ffc RDI: ffffa39b3b116ffc
    Apr 29 13:12:24 sf kernel: RBP: ffffa39b3ac04400 R08: ffffa39b3b802f00 R09: 00000000fffff73b
    Apr 29 13:12:24 sf kernel: R10: ffffffffbe2b6e51 R11: 00505b1b004d5b1b R12: 0000000000000000
    Apr 29 13:12:24 sf kernel: R13: ffffa39b39c9b087 R14: 0000000000000018 R15: ffffa39b39c9b08c
    Apr 29 13:12:24 sf kernel: FS:  00007f84c341e580(0000) GS:ffffa39b3f1c0000(0000) knlGS:0000000000000000
    Apr 29 13:12:24 sf kernel: CS:  0010 DS: 0000 ES: 0000 CR0: 0000000080050033
    Apr 29 13:12:24 sf kernel: CR2: ffffa39b3b117000 CR3: 00000007e9d42003 CR4: 00000000000606e0
    Apr 29 13:12:24 sf kernel: Call Trace:
    Apr 29 13:12:24 sf kernel:  vt_do_kdgkb_ioctl+0x352/0x450
    Apr 29 13:12:24 sf kernel:  vt_ioctl+0xba3/0x1190
    Apr 29 13:12:24 sf kernel:  ? __bpf_prog_run32+0x39/0x60
    Apr 29 13:12:24 sf kernel:  ? trace_hardirqs_on+0x31/0xe0
    Apr 29 13:12:24 sf kernel:  tty_ioctl+0x23f/0x920
    Apr 29 13:12:24 sf kernel:  ? preempt_count_sub+0x98/0xe0
    Apr 29 13:12:24 sf kernel:  ? __seccomp_filter+0xc2/0x450
    Apr 29 13:12:24 sf kernel:  ? __handle_mm_fault+0x7b0/0x1530
    Apr 29 13:12:24 sf kernel:  do_vfs_ioctl+0xa2/0x6a0
    Apr 29 13:12:24 sf kernel:  ? syscall_trace_enter+0x126/0x280
    Apr 29 13:12:24 sf kernel:  ksys_ioctl+0x3a/0x70
    Apr 29 13:12:24 sf kernel:  __x64_sys_ioctl+0x16/0x20
    Apr 29 13:12:24 sf kernel:  do_syscall_64+0x54/0xe0
    Apr 29 13:12:24 sf kernel:  entry_SYSCALL_64_after_hwframe+0x49/0xbe
    Apr 29 13:12:24 sf kernel: RIP: 0033:0x7f84c334a3b7
    Apr 29 13:12:24 sf kernel: Code: 00 00 00 75 0c 48 c7 c0 ff ff ff ff 48 83 c4 18 c3 e8 dd d2 01 00 66 2e 0f 1f 84 00 00 00 00 00 0f 1f 00 b8 10 00 00 00 0f 05 <48> 3d 01 f0 ff ff 73 01 c3 48 8b 0d a9 ca 0c 00 f7 d8 64 $
    Apr 29 13:12:24 sf kernel: RSP: 002b:00007ffed2cc88f8 EFLAGS: 00000246 ORIG_RAX: 0000000000000010
    Apr 29 13:12:24 sf kernel: RAX: ffffffffffffffda RBX: 0000000000000018 RCX: 00007f84c334a3b7
    Apr 29 13:12:24 sf kernel: RDX: 00007ffed2cc8910 RSI: 0000000000004b49 RDI: 0000000000000003
    Apr 29 13:12:24 sf kernel: RBP: 00007ffed2cc8911 R08: 00007f84c3417c40 R09: 0000561cb25db4a0
    Apr 29 13:12:24 sf kernel: R10: 0000000000000000 R11: 0000000000000246 R12: 0000561cb25d32b0
    Apr 29 13:12:24 sf kernel: R13: 00007ffed2cc8910 R14: 0000000000000018 R15: 0000000000000003
    Apr 29 13:12:24 sf kernel: Modules linked in: sit tunnel4 ip_tunnel snd_hda_codec_hdmi snd_hda_codec_via snd_hda_codec_generic snd_hda_intel snd_hda_codec r8712u(C) snd_hwdep ath9k_htc snd_hda_core ath9k_common ath9k_h$
    Apr 29 13:12:24 sf kernel: CR2: ffffa39b3b117000
    Apr 29 13:12:24 sf kernel: ---[ end trace 9c4dbd36dd993d54 ]---
    Apr 29 13:12:24 sf kernel: RIP: 0010:__memmove+0x81/0x1a0
    Apr 29 13:12:24 sf kernel: Code: 4c 89 4f 10 4c 89 47 18 48 8d 7f 20 73 d4 48 83 c2 20 e9 a2 00 00 00 66 90 48 89 d1 4c 8b 5c 16 f8 4c 8d 54 17 f8 48 c1 e9 03 <f3> 48 a5 4d 89 1a e9 0c 01 00 00 0f 1f 40 00 48 89 d1 4c $
    Apr 29 13:12:24 sf kernel: RSP: 0018:ffffc0c3c0c7fd08 EFLAGS: 00010203
    Apr 29 13:12:24 sf kernel: RAX: ffffa39b39c9b08c RBX: 0000000000000019 RCX: 00000b8c90633fcb
    Apr 29 13:12:24 sf kernel: RDX: 00005c648461bdcd RSI: ffffa39b3b116ffc RDI: ffffa39b3b116ffc
    Apr 29 13:12:24 sf kernel: RBP: ffffa39b3ac04400 R08: ffffa39b3b802f00 R09: 00000000fffff73b
    Apr 29 13:12:24 sf kernel: R10: ffffffffbe2b6e51 R11: 00505b1b004d5b1b R12: 0000000000000000
    Apr 29 13:12:24 sf kernel: R13: ffffa39b39c9b087 R14: 0000000000000018 R15: ffffa39b39c9b08c
    Apr 29 13:12:24 sf kernel: FS:  00007f84c341e580(0000) GS:ffffa39b3f1c0000(0000) knlGS:0000000000000000
    Apr 29 13:12:24 sf kernel: CS:  0010 DS: 0000 ES: 0000 CR0: 0000000080050033
    Apr 29 13:12:24 sf kernel: CR2: ffffa39b3b117000 CR3: 00000007e9d42003 CR4: 00000000000606e0
    Apr 29 13:12:24 sf kernel: BUG: sleeping function called from invalid context at include/linux/percpu-rwsem.h:34
    Apr 29 13:12:24 sf kernel: in_atomic(): 0, irqs_disabled(): 1, pid: 423, name: loadkeys
    Apr 29 13:12:24 sf kernel: CPU: 7 PID: 423 Comm: loadkeys Tainted: G      D  C        5.1.0-rc7 #98
    Apr 29 13:12:24 sf kernel: Hardware name: Gigabyte Technology Co., Ltd. To be filled by O.E.M./H77M-D3H, BIOS F12 11/14/2013
    Apr 29 13:12:24 sf kernel: Call Trace:
    Apr 29 13:12:24 sf kernel:  dump_stack+0x67/0x90
    Apr 29 13:12:24 sf kernel:  ? wake_up_klogd+0x10/0x70
    Apr 29 13:12:24 sf kernel:  ___might_sleep.cold.18+0xd4/0xe4
    Apr 29 13:12:24 sf kernel:  exit_signals+0x1c/0x200
    Apr 29 13:12:24 sf kernel:  do_exit+0xa8/0xbb0
    Apr 29 13:12:24 sf kernel:  ? ksys_ioctl+0x3a/0x70
    Apr 29 13:12:24 sf kernel:  rewind_stack_do_exit+0x17/0x20

These backtraces did not prevent machine from booting and did not
seem to cause any ill immediate effect. But they still looked very
scary: something failed to copy data somewhere after all, that
meant certain corruption.

This trace says that **loadkeys** program managed to crash the kernel by calling
an **ioctl** syscall(**__x64_sys_ioctl**) and that crash happens somewhere in
**memmove()** function.

Sounds like a very strange bug to have. What could **loadkeys** do so
complicated to get kernel confused? It's `whole source <https://github.com/legionus/kbd/blob/master/src/loadkeys.c>`_
is 200 lines. Well, actual key loading happens `here <https://github.com/legionus/kbd/blob/master/src/libkeymap/loadkeys.c#L18>`_
via **ioctl(KDSKBMODE)** and **ioctl(KDSKBENT)**.

Searching internet for
**__memmove+loadkeys** showsh that people are occasionally seeing
these crashes since at least 2009 (kernel **4.1**). I encountered
no conclusive investigations and dived in.

The backtrace above suggests crash happened somewhere at
`vt_do_kdgkb_ioctl() <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/tty/vt/keyboard.c?h=v5.1&id=e93c9c99a629c61837d5a7fc2120cd2b6c70dbdd#n1986>`_:

.. code-block:: c

    /* FIXME: This one needs untangling and locking */
    int vt_do_kdgkb_ioctl(int cmd, struct kbsentry __user * user_kdgkb, int perm)
    {
    	struct kbsentry *kbs;
    	char *p;
    	u_char *q;
    	u_char __user *up;
    	int sz;
    	int delta;
    	char *first_free, *fj, *fnw;
    	int i, j, k;
    	int ret;
    
    	if (!capable(CAP_SYS_TTY_CONFIG))
    		perm = 0;
    
    	kbs = kmalloc(sizeof(*kbs), GFP_KERNEL);
    	if (!kbs) {
    		ret = -ENOMEM;
    		goto reterr;
    	}
    
    	/* we mostly copy too much here (512bytes), but who cares ;) */
    	if (copy_from_user(kbs, user_kdgkb, sizeof(struct kbsentry))) {
    		ret = -EFAULT;
    		goto reterr;
    	}
    	kbs->kb_string[sizeof(kbs->kb_string) - 1] = '\0';
    	i = kbs->kb_func;
    
    	switch (cmd) {
    	case KDGKBSENT:
    		sz = sizeof(kbs->kb_string) - 1;	/* sz should have been
    							   a struct member */
    		up = user_kdgkb->kb_string;
    		p = func_table[i];
    		if (p)
    			for (; *p && sz; p++, sz--)
    				if (put_user(*p, up++)) {
    					ret = -EFAULT;
    					goto reterr;
    				}
    		if (put_user('\0', up)) {
    			ret = -EFAULT;
    			goto reterr;
    		}
    		kfree(kbs);
    		return ((p && *p) ? -EOVERFLOW : 0);
    	case KDSKBSENT:
    		if (!perm) {
    			ret = -EPERM;
    			goto reterr;
    		}
    
    		q = func_table[i];
    		first_free = funcbufptr + (funcbufsize - funcbufleft);
    		for (j = i + 1; j < MAX_NR_FUNC && !func_table[j]; j++) ;
    		if (j < MAX_NR_FUNC)
    			fj = func_table[j];
    		else
    			fj = first_free;
    
    		delta = (q ? -strlen(q) : 1) + strlen(kbs->kb_string);
    		if (delta <= funcbufleft) {	/* it fits in current buf */
    			if (j < MAX_NR_FUNC) {
    				memmove(fj + delta, fj, first_free - fj);
    				for (k = j; k < MAX_NR_FUNC; k++)
    					if (func_table[k])
    						func_table[k] += delta;
    			}
    			if (!q)
    				func_table[i] = fj;
    			funcbufleft -= delta;
    		} else {	/* allocate a larger buffer */
    			sz = 256;
    			while (sz < funcbufsize - funcbufleft + delta)
    				sz <<= 1;
    			fnw = kmalloc(sz, GFP_KERNEL);
    			if (!fnw) {
    				ret = -ENOMEM;
    				goto reterr;
    			}
    
    			if (!q)
    				func_table[i] = fj;
    			if (fj > funcbufptr)
    				memmove(fnw, funcbufptr, fj - funcbufptr);
    			for (k = 0; k < j; k++)
    				if (func_table[k])
    					func_table[k] =
    					    fnw + (func_table[k] - funcbufptr);
    
    			if (first_free > fj) {
    				memmove(fnw + (fj - funcbufptr) + delta, fj,
    					first_free - fj);
    				for (k = j; k < MAX_NR_FUNC; k++)
    					if (func_table[k])
    						func_table[k] =
    						    fnw + (func_table[k] -
    							   funcbufptr) + delta;
    			}
    			if (funcbufptr != func_buf)
    				kfree(funcbufptr);
    			funcbufptr = fnw;
    			funcbufleft = funcbufleft - delta + sz - funcbufsize;
    			funcbufsize = sz;
    		}
    		strcpy(func_table[i], kbs->kb_string);
    		break;
    	}
    	ret = 0;
     reterr:
    	kfree(kbs);
    	return ret;
    }

It's a huge function but it's high-level purpose is simple:

- handle **ioctl(KDGKBSENT)** call (Get KeyBoard Entries)
- handle **ioctl(KDSKBSENT)** call (Set KeyBoard Entries)

Entries are `struct kbsentry <>`_:

.. code-block:: C

    struct kbsentry {
        unsigned char kb_func;
        unsigned char kb_string[512];
    };

All it does is to substitute input char **kb_func** for a sequence
of chars as **kb_string** (they can be scape sequences understood
by linux terminal).

**KDSKBSENT** handler above is full of array handling logic. To
understand is we need to look at the actual data structures in
`drivers/tty/vt/defkeymap.c_shipped <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/tty/vt/defkeymap.c_shipped?h=v5.1&id=e93c9c99a629c61837d5a7fc2120cd2b6c70dbdd>`_:

.. code-block:: c

    /* Do not edit this file! It was automatically generated by   */
    /*    loadkeys --mktable defkeymap.map > defkeymap.c          */
    
    #include <linux/types.h>
    #include <linux/keyboard.h>
    #include <linux/kd.h>
    
    ...
    
    /*
     * Philosophy: most people do not define more strings, but they who do
     * often want quite a lot of string space. So, we statically allocate
     * the default and allocate dynamically in chunks of 512 bytes.
     */
    
    char func_buf[] = {
     '\033', '[', '[', 'A', 0, 
     '\033', '[', '[', 'B', 0, 
     '\033', '[', '[', 'C', 0, 
     '\033', '[', '[', 'D', 0, 
     '\033', '[', '[', 'E', 0, 
     '\033', '[', '1', '7', '~', 0, 
     '\033', '[', '1', '8', '~', 0, 
     '\033', '[', '1', '9', '~', 0, 
     '\033', '[', '2', '0', '~', 0, 
     '\033', '[', '2', '1', '~', 0, 
     '\033', '[', '2', '3', '~', 0, 
     '\033', '[', '2', '4', '~', 0, 
     '\033', '[', '2', '5', '~', 0, 
     '\033', '[', '2', '6', '~', 0, 
     '\033', '[', '2', '8', '~', 0, 
     '\033', '[', '2', '9', '~', 0, 
     '\033', '[', '3', '1', '~', 0, 
     '\033', '[', '3', '2', '~', 0, 
     '\033', '[', '3', '3', '~', 0, 
     '\033', '[', '3', '4', '~', 0, 
     '\033', '[', '1', '~', 0, 
     '\033', '[', '2', '~', 0, 
     '\033', '[', '3', '~', 0, 
     '\033', '[', '4', '~', 0, 
     '\033', '[', '5', '~', 0, 
     '\033', '[', '6', '~', 0, 
     '\033', '[', 'M', 0, 
     '\033', '[', 'P', 0, 
    };
    
    char *funcbufptr = func_buf;
    int funcbufsize = sizeof(func_buf);
    int funcbufleft = 0;          /* space left */
    
    char *func_table[MAX_NR_FUNC] = {
     func_buf + 0,
     func_buf + 5,
     func_buf + 10,
     func_buf + 15,
     func_buf + 20,
     func_buf + 25,
     func_buf + 31,
     func_buf + 37,
     func_buf + 43,
     func_buf + 49,
     func_buf + 55,
     func_buf + 61,
     func_buf + 67,
     func_buf + 73,
     func_buf + 79,
     func_buf + 85,
     func_buf + 91,
     func_buf + 97,
     func_buf + 103,
     func_buf + 109,
     func_buf + 115,
     func_buf + 120,
     func_buf + 125,
     func_buf + 130,
     func_buf + 135,
     func_buf + 140,
     func_buf + 145,
     NULL,
     NULL,
     func_buf + 149,
     NULL,
    };

Here we can see that **func_buf** is statically allocated flattened
array of default keymaps. **func_table** array of pointers is a fast
lookup table into flat **func_buf** array. If **func_buf** has not
enough space it gets reallocated at **funcbufptr**.

That's why **vt_do_kdgkb_ioctl()** is so complicated: it patches and
update all these offsets.

Also note: **func_buf** and **funcbufptr** are both global pointers
without any locking around these globals (also stressed by a **FIXME**
above).

This is our somewhat smoking gun: if something in my system happens to
call **ioctl(KDSKBSENT)** in parallel on multiple CPUs it will be able
to mess up **func_table** into something that does not make sense. That
can lead to strange things when you press these keys!

The only problem was that normally you have only one **loadkeys**
being ran for a short time when your system boots up. Nothing else
should be touching keymaps at that time anyway (or after).

Into the rabbit hole
--------------------

To validate the race theory I added debug statement into
**vt_do_kdgkb_ioctl()** function to see who calls it at boot:

.. code-block:: diff

    --- a/drivers/tty/vt/keyboard.c
    +++ b/drivers/tty/vt/keyboard.c
    @@ -1996,6 +1996,14 @@ int vt_do_kdgkb_ioctl(int cmd, struct kbsentry __user *user_kdgkb, int perm)
            int i, j, k;
            int ret;
     
    +       printk("In vt_do_kdgkb_ioctl(%d=%s)/cpu=%d/comm=%s(%d)\n",
    +               cmd, (cmd == KDGKBSENT)
    +                       ? "KDGKBSENT"
    +                       : ((cmd == KDSKBSENT)
    +                               ? "KDSKBSENT"
    +                               : "UNKNOWN"),
    +               hard_smp_processor_id(), current->comm, task_pid_nr(current));
    +
            if (!capable(CAP_SYS_TTY_CONFIG))
                    perm = 0;

.. code-block::

    Feb 24 12:06:35 sf systemd-vconsole-setup[343]: Executing "/usr/bin/loadkeys -q -C /dev/tty1 -u ru4"...
    Feb 24 12:06:35 sf systemd-vconsole-setup[344]: /usr/bin/setfont succeeded.
    Feb 24 12:06:35 sf systemd-vconsole-setup[344]: Executing "/usr/bin/loadkeys -q -C /dev/tty1 -u ru4"...
    Feb 24 12:06:35 sf systemd-vconsole-setup[343]: Successfully forked off '(loadkeys)' as PID 423.
    Feb 24 12:06:35 sf systemd-vconsole-setup[344]: Successfully forked off '(loadkeys)' as PID 424.
    ...
    Feb 24 12:06:35 sf kernel: In vt_do_kdgkb_ioctl(19273=KDSKBSENT)/cpu=5/comm=loadkeys(424)
    Feb 24 12:06:35 sf kernel: In vt_do_kdgkb_ioctl(19273=KDSKBSENT)/cpu=2/comm=loadkeys(423)
    ...
    <more of these with interleaved PIDs>

Bingo: systemd was running exactly two instances of **loadkeys** at the same
time: **loadkeys(424)** and **loadkeys(423)**. It's an ideal way to trigger
the race: two processes are likely blocked by IO as they are executed for
the first time from disk, and once unblocked execute exactly the same code
in parallel instruction for instruction.

But why does systemd runs loadkeys twice? Why not once or as many times as I
have ttys?

For many systems it's supposed to happen only once. See
`90-vconsole.rules udev rule <https://github.com/systemd/systemd/blob/883eb9be985fd86d9cabe967eeeab91cdd396a81/src/vconsole/90-vconsole.rules.in#L12>`_:

.. code-block::

    # Each vtcon keeps its own state of fonts.
    #
    ACTION=="add", SUBSYSTEM=="vtconsole", KERNEL=="vtcon*", RUN+="@rootlibexecdir@/systemd-vconsole-setup"

Normally you have only one **/sys/devices/virtual/vtconsole/vtcon0**. But my system has two of these:

.. code-block::

    # cat /sys/devices/virtual/vtconsole/vtcon0/name
    (S) dummy device
    # cat /sys/devices/virtual/vtconsole/vtcon1/name
    (M) frame buffer device

That dummy console comes from **intel** framebuffer driver:

.. code-block:: c

    // somewhere in drivers/gpu/drm/i915/i915_drv.c:
    ret = do_take_over_console(&dummy_con, 0, MAX_NR_CONSOLES - 1, 1);

**i915** is an **intel** VGA video driver. My system has this driver
compiled into kernel. That triggers kernel to discover and expose
**vtcon0**/**vtcon1** at the same time.

My speculation is that for non-intel-video systems (or for systems with
intel driver loaded at a late stage) the condition might not trigger at
all because those get only one **loadkeys** run (or a few runs spanned
in time after each module is loaded).

The fix `was simple <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/drivers/tty/vt?id=46ca3f735f345c9d87383dd3a09fa5d43870770e>`_:
add some locking at least for write/write race. I did not touch read
paths as I was not sure which subsystems use **vt** subsystem. Maybe
some of them require decent throughput and lock for every character
would be too much.

After this patch applied I had no bactraces at boot and no more
unexpected poweroffs. But who knows, maybe it was a distraction
and power button can't be simulated through any tty escapes. We'll see.

If you are wondering what you could fix yourself in linux kernel you
can finish this work and also add read/write locking!

Parting words
-------------

- The possible cause of spurious reboots was data corruption caused by
  very old race condiiton in kernel.
- Silent data corruption is hard to diagnose if you don't know where to
  look. I was lucky to get a kernel oops in the same buggy code.
- **tty/vt** driver is full of globals. Those should perhaps be changed to
  be per-vtcon arrays (some non-**x86** already have it tht way).
- **tty/vt** global tables are actually generated by an old userspace tool
  **loadkeys \-\-mktable** tool and stored in kernel as-is.
- There is still a read/write race in kernel waiting for you to fix it!

Have fun!
