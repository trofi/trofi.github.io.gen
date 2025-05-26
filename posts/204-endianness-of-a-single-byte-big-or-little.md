---
title: "Endianness of a single byte: big or little?"
date: November 18, 2017
---

## Bug

Rolf Eike Beer noticed two test failures while testing `radvd` package
(`IPv6` route advertiser daemon and more) on `sparc`. Both tests
failed likely due to endianness issue:

``` 
test/send.c:317:F:build:test_add_ra_option_lowpanco:0:
  Assertion '0 == memcmp(expected, sb.buffer, sizeof(expected))'
    failed: 0 == 0, memcmp(expected, sb.buffer, sizeof(expected)) == 1
test/send.c:342:F:build:test_add_ra_option_abro:0:
  Assertion '0 == memcmp(expected, sb.buffer, sizeof(expected))'
    failed: 0 == 0, memcmp(expected, sb.buffer, sizeof(expected)) == 1
```

I've confirmed the same failure on `powerpc`.
Eike noted that it's unusual because `sparc` is a big-endian architecture
and network byte order is also big-endian (thus no need to flip bytes).
Something very specific must have lurked in `radvd` code to break
endianness in this case. Curiously all the `radvd` tests were working
fine on `amd64`.
Two functions failed to produce expected results:

``` c
START_TEST(test_add_ra_option_lowpanco)
{
        ck_assert_ptr_ne(0, iface);

        struct safe_buffer sb = SAFE_BUFFER_INIT;
        add_ra_option_lowpanco(&sb, iface->AdvLowpanCoList);

        unsigned char expected[] = {
            0x22, 0x03, 0x32, 0x48, 0x00, 0x00, 0xe8, 0x03, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        };

        ck_assert_int_eq(sb.used, sizeof(expected));
        ck_assert_int_eq(0, memcmp(expected, sb.buffer, sizeof(expected)));

        safe_buffer_free(&sb);
}
END_TEST

START_TEST(test_add_ra_option_abro)
{
        ck_assert_ptr_ne(0, iface);

        struct safe_buffer sb = SAFE_BUFFER_INIT;
        add_ra_option_abro(&sb, iface->AdvAbroList);

        unsigned char expected[] = {
            0x23, 0x03, 0x0a, 0x00, 0x02, 0x00, 0x02, 0x00, 0xfe, 0x80, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0xa2, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
        };

        ck_assert_int_eq(sb.used, sizeof(expected));
        ck_assert_int_eq(0, memcmp(expected, sb.buffer, sizeof(expected)));

        safe_buffer_free(&sb);
}
END_TEST
```

Both tests are straightforward: they verify `6LoWPAN` and `ABRO`
extension handling (both are related to route announcement for Low Power
devices). Does not look complicated.
I looked at `add_ra_option_lowpanco()` implementation and noticed at
least one bug of missing endianness conversion:

``` c
// radvd.h
struct nd_opt_abro {
        uint8_t nd_opt_abro_type;
        uint8_t nd_opt_abro_len;
        uint16_t nd_opt_abro_ver_low;
        uint16_t nd_opt_abro_ver_high;
        uint16_t nd_opt_abro_valid_lifetime;
        struct in6_addr nd_opt_abro_6lbr_address;
};
// ...
// send.c
static void add_ra_option_mipv6_home_agent_info(struct safe_buffer *sb, struct mipv6 const *mipv6)
{
        struct HomeAgentInfo ha_info;

        memset(&ha_info, 0, sizeof(ha_info));

        ha_info.type = ND_OPT_HOME_AGENT_INFO;
        ha_info.length = 1;
        ha_info.flags_reserved = (mipv6->AdvMobRtrSupportFlag) ? ND_OPT_HAI_FLAG_SUPPORT_MR : 0;
        ha_info.preference = htons(mipv6->HomeAgentPreference);
        ha_info.lifetime = htons(mipv6->HomeAgentLifetime);

        safe_buffer_append(sb, &ha_info, sizeof(ha_info));
}

static void add_ra_option_abro(struct safe_buffer *sb, struct AdvAbro const *abroo)
{
        struct nd_opt_abro abro;

        memset(&abro, 0, sizeof(abro));

        abro.nd_opt_abro_type = ND_OPT_ABRO;
        abro.nd_opt_abro_len = 3;
        abro.nd_opt_abro_ver_low = abroo->Version[1];
        abro.nd_opt_abro_ver_high = abroo->Version[0];
        abro.nd_opt_abro_valid_lifetime = abroo->ValidLifeTime;
        abro.nd_opt_abro_6lbr_address = abroo->LBRaddress;

        safe_buffer_append(sb, &abro, sizeof(abro));
}
```

Note how `add_ra_option_mipv6_home_agent_info()` carefully flips bytes
with `htons()` for all `uint16_t` fields but
`add_ra_option_abro()` does not.
It means the `ABRO` does not really work on little-endian (aka most)
systems in `radvd` and test checks for the wrong thing. I added
missing `htons()` calls and fixed `expected[]` output in tests by
manually flipping two bytes in a few locations.

## Plot twist

The effect was slightly unexpected: I fixed only `ABRO` test, but not
`6LoWPAN`. It's where things became interesting. Let's look at
`add_ra_option_lowpanco()` implementation:

``` c
// radvd.h
struct nd_opt_6co {
        uint8_t nd_opt_6co_type;
        uint8_t nd_opt_6co_len;
        uint8_t nd_opt_6co_context_len;
        uint8_t nd_opt_6co_res : 3;
        uint8_t nd_opt_6co_c : 1;
        uint8_t nd_opt_6co_cid : 4;
        uint16_t nd_opt_6co_reserved;
        uint16_t nd_opt_6co_valid_lifetime;
        struct in6_addr nd_opt_6co_con_prefix;
};
// ...
// send.c
static void add_ra_option_lowpanco(struct safe_buffer *sb, struct AdvLowpanCo const *lowpanco)
{
        struct nd_opt_6co co;

        memset(&co, 0, sizeof(co));

        co.nd_opt_6co_type = ND_OPT_6CO;
        co.nd_opt_6co_len = 3;
        co.nd_opt_6co_context_len = lowpanco->ContextLength;
        co.nd_opt_6co_c = lowpanco->ContextCompressionFlag;
        co.nd_opt_6co_cid = lowpanco->AdvContextID;
        co.nd_opt_6co_valid_lifetime = lowpanco->AdvLifeTime;
        co.nd_opt_6co_con_prefix = lowpanco->AdvContextPrefix;

        safe_buffer_append(sb, &co, sizeof(co));
}
```

The test still failed to match one single byte: the one that spans 3
bit fields: `nd_opt_6co_res`, `nd_opt_6co_c`, `nd_opt_6co_cid`. But
why? Does endianness really matter within byte? Apparently `gcc` happens
to group those 3 fields in different orders on `x86_64` and
`powerpc`!
Let's looks at a smaller example:

``` c
#include <stdio.h>
#include <stdint.h>

struct s {
    uint8_t a : 3;
    uint8_t b : 1;
    uint8_t c : 4;
};

int main() {
    struct s v = { 0x00, 0x1, 0xF, };

    printf("v = %#02x\n", *(uint8_t*)&v);
    return 0;
}
```

Output difference:

```
$ x86_64-pc-linux-gnu-gcc a.c -o a && ./a
v = 0xf8
# (0xF << 5) | (0x1 << 4) | 0x00

$ powerpc-unknown-linux-gnu-gcc a.c -o a && ./a
v = 0x1f
# (0x0 << 5) | (0x1 << 4) | 0xF
```

C standard does not specify the layout of bit fields and it's a great
illustration of how things break :)
An interesting observation: the bit field order on `powerpc` happens to
be the desired order (as `6LoWPAN` `RFC` defines it).
It means `radvd` code indeed happened to generate correct bitstream on
big-endian platforms (as Eike predicted) but did not work on
little-endian systems. Unfortunately golden `expected[]` output was
generated on little-endian system.

Thus the 3 fixes:

- use `uint8_t` instead of bit field (and tweak the golden byte):
  <https://github.com/reubenhwk/radvd/pull/78/commits/25f627b3ae878cab1c5362ba70947e1bf8a250c4>
- regenerate golden output on big-endian system:
  <https://github.com/reubenhwk/radvd/pull/78/commits/6b6f81e2979a3cd266e2fce84044485300460767>
- fix missing `uint16_t` handling:
  <https://github.com/reubenhwk/radvd/pull/78/commits/dac7758152188aed9bef140ca2132271e77dfa74>

That's it :)
