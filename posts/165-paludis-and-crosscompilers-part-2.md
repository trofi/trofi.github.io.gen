---
title: paludis and crosscompilers (part 2)
date: December 20, 2011
---

Я думал, что собирать остальные кросскомпиляторы так же просто, как и
[`arm`](/posts/127-paludis-and-crosscompilers.html)
кросскомпилятор. Ан нет! `paludis` запас нам несколько подводных
граблей.

Казалось бы, чего проще - собрать `powerpc64-unknown-linux-gnu`
`toolchain`. По аналогии с предыдущим доком создаем группу
`cross-powerpc64-unknown-linux-gnu` и собираем всё в том же порядке.
Но при сборке реальной `glibc` нас ожидает проблема: в `gcc`
просачиваются `CFLAGS` нашего `host` компилятора.

Между `emerge` и `paludis` есть одно очень серьезное отличие:

```
22:08 < ciaranm> you might find that this is one of those situations
                 where it works with portage because por tage randomly
                 doesn't 'export' certain variables, and just has them
                 as globals
```

Казалось бы, подумаешь какая-то переменная попадает в окружение дочерним
процессам. Оказывается, в `gcc` (с виду `gentoo` специфично) есть
скрытая фича: протаскивание `CFLAGS` в `gcc` через переменные
среды!:

```
$ x86_64-pc-linux-gnu-gcc a.c -o a
$ file a
    a: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.9, not stripped
# всё хорошо
#
$ ABI=n32 CFLAGS_n32=-mabi=n32 x86_64-pc-linux-gnu-gcc a.c -o a
    a.c:1:0: error: unknown ABI (n32) for -mabi= switch
# сюрприз!
```

Всё бы хорошо, но `ABI` - переменная, которая попадает в ебилды из
`profiles/` и `paludis` ее экспортирует, а `emerge` - нет. В итоге
все программы собираются чуть по-разному.

В прошлый раз я это "починил" это не понимая выставив `ABI=cross`,

Теперь рассмотрим чего еще не хватает.

Иногда `eclass/multilib.eclass` таки выставляет нужный `ABI` при
сборке:

``` bash
multilib_toolchain_setup() {
    local v vv
    export ABI=$1
...
```

Но эта функция никогда не вызывается для самих `toolchain` пакетов,
так как там в каждой фазе (например, в `src_compile`) фигурирует сразу
пачка `ABI` одновременно (host `ABI`, и несколько target `ABI`).

Единственная мелочь, которой нам не хватает для сборки `cross`
`multilib` компилятора - это список `ABI`, которые мы хотим видеть в
результате; и `ABI` кросскомпилятора по умолчанию.

Этим занимаются 2 переменные: `MULTILIB_ABIS` и `DEFAULT_ABI` (обе
выставляются в `profiles/`, так что они просачиваются в наше окружение
из хост системы).

Так что теперь `bashrc.cross` у меня теперь выглядит так:

``` bash
case "${CATEGORY}" in
    cross-*)
        # cross-build
        CBUILD="${CHOST}"
        CTARGET="${CATEGORY#cross-}"
        #
        # самая важная строка :]
        unset ABI
        #
        #
        # but paludis sets them back to default too early
        # it's fine if nonmultilib ABI is enough for you
        # copied from multilib.eclass:multilib_env()
        case "${CTARGET}" in
            x86_64*)
                #MULTILIB_ABIS="x32 amd64 x86"
                MULTILIB_ABIS="x32"
                DEFAULT_ABI="x32"
                ;;
            mips64*)
                MULTILIB_ABIS="n64 n32 o32"
                DEFAULT_ABI="n32"
                ;;
            powerpc64*)
                MULTILIB_ABIS="ppc64 ppc"
                DEFAULT_ABI="ppc64"
                ;;
            s390x*)
                MULTILIB_ABIS="s390x s390"
                DEFAULT_ABI="s390x"
                ;;
            sparc64*)
                MULTILIB_ABIS="sparc64 sparc32"
                DEFAULT_ABI="sparc64"
                ;;
            *)
                DEFAULT_ABI="default"
                MULTILIB_ABIS="default"
                ;;
        esac
        #
        # просто баг в src_install у glibc
        unset SYMLINK_LIB
        #
        case "${PN}" in
            binutils|gcc|gdb)
                :
            ;;
            *)
                CC="${CTARGET}-gcc"
                CXX="${CTARGET}-g++"
                LD="${CTARGET}-ld"
            ;;
        esac
    ;;
esac
```

Тут мы видим прикольный пример: кросскомпилятор на `x86_64` `host` в
`x86_64` `target`:

- `host` компилятор: `x86_64-pc-linux-gnu`
- `target` компилятор: `x86_64-unknown-linux-gnu`

```
# target compiler
$ x86_64-unknown-linux-gnu-gcc a.c -o a
$ file a
    a: ELF 32-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.35, not stripped
# host compiler
$ x86_64-pc-linux-gnu-gcc a.c -o a
$ file a
    a: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.9, not stripped
```

Чего я насобирал пока изучал как в `gentoo` всё это дело собирается:

```
$ gcc-config -l
  [1] alpha-unknown-linux-gnu-4.5.3 *
  [2] armv5tel-softfloat-linux-gnueabi-4.5.3 *
  [3] armv7a-unknown-linux-gnueabi-4.5.3 *
  [4] ia64-unknown-linux-gnu-4.5.3 *
  [5] mingw32-4.5.3 *
  [6] mips64-unknown-linux-gnu-4.6.2 *
  [7] powerpc64-unknown-linux-gnu-4.5.3 *
  [8] s390x-unknown-linux-gnu-4.5.3 *
  [9] sparc64-unknown-linux-gnu-4.5.3 *
  [10] sparc-unknown-linux-gnu-4.5.3 *
  [11] x86_64-pc-linux-gnu-4.5.3 *
  [12] x86_64-pc-linux-gnu-4.6.2
  [13] x86_64-unknown-linux-gnu-4.6.2 *
```

И все работают!

Если ооооочень хочется установить `SYMLINK_LIB=yes`, то нужно
подфиксить `src_install` `glibc`, иначе он сломает хостовый `/lib`
(`vapier` грозился пофиксить это сам):

```diff
RCS file: /var/cvsroot/gentoo-x86/sys-libs/glibc/files/eblits/src_install.eblit,v
retrieving revision 1.19
diff -u -u -r1.19 src_install.eblit
--- files/eblits/src_install.eblit      12 Dec 2011 17:33:10 -0000      1.19
+++ files/eblits/src_install.eblit      20 Dec 2011 18:31:14 -0000
@@ -10,8 +10,9 @@
                GBUILDDIR=${WORKDIR}/build-${ABI}-${CTARGET}-nptl
        fi
-       local install_root=${D}
-       is_crosscompile && install_root="${install_root}/usr/${CTARGET}"
+       local cross_prefix=
+       is_crosscompile && cross_prefix=/usr/${CTARGET}
+       local install_root=${D}${cross_prefix}
        if want_linuxthreads ; then
                cd "${WORKDIR}"/build-${ABI}-${CTARGET}-linuxthreads
                einfo "Installing GLIBC ${ABI} with linuxthreads ..."
@@ -74,12 +75,12 @@
        if [[ ${SYMLINK_LIB} == "yes" ]] && has_multilib_profile ; then
                case $(tc-arch) in
                        amd64)
-                               [[ ! -e ${D}/lib ]] && dosym $(get_abi_LIBDIR ${DEFAULT_ABI}) /lib
-                               dosym ../$(get_abi_LIBDIR x86)/ld-linux.so.2 /lib/ld-linux.so.2
+                               [[ ! -e ${install_root}/lib ]] && dosym $(get_abi_LIBDIR ${DEFAULT_ABI}) ${cross_prefix}/lib
+                               dosym ../$(get_abi_LIBDIR x86)/ld-linux.so.2 ${cross_prefix}/lib/ld-linux.so.2
                                ;;
                        ppc64)
-                               [[ ! -e ${D}/lib ]] && dosym $(get_abi_LIBDIR ${DEFAULT_ABI}) /lib
-                               dosym ../$(get_abi_LIBDIR ppc)/ld.so.1 /lib/ld.so.1
+                               [[ ! -e ${install_root}/lib ]] && dosym $(get_abi_LIBDIR ${DEFAULT_ABI}) ${cross_prefix}/lib
+                               dosym ../$(get_abi_LIBDIR ppc)/ld.so.1 ${cross_prefix}/lib/ld.so.1
                                ;;
                esac
        fi
```
