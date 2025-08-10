---
title: "cooling failure"
date: February 25, 2023
---

About 2 years ago I got my new desktop. It's a 16-core AMD CPU with
128GB RAM. My plan was to compile A Lot :)

I was not sure how much of a heat sink I need for that machine. My
previous CPU had about `125W` power consumption and could get away with
a silent rotary air cooler.
This machine seems to dissipate up to `150W` and I was not sure if it's a
peak value or some average that I could consistently exceed with heavy
workloads.

I decided to give water cooling system a try for the first time. I
expected it to be less noisy compared to rotary air ones.
When my desktop just arrived I was surprised to see that water cooling
system used not just one but two fans! (And two more system fans).

When I powered the machine up for the first time I heard a nice low
frequency humming noise. It went up a bit when I applied `100%` CPU load
workloads. It was not too distracting but a bit disappointing that I
can hear when the machine is under the load or not.

2 years passed. I compiled a lot and caught many bugs in various
software projects. I had no issue with the hardware.

And one day my machine just powered off abruptly. I powered it on again.
It ran for 2-3 minutes, and shut off again. I was not able to power it on
later without a 30-minute delay. Initially I though about failed PSU as
the system did not complain about anything up to the very shutdown.

Next day I managed to get into `EFI` menu to look at CPU statistics.
`EFI` dashboard rendered the menacing temperature graph: it went from
60 degrees to 110 for a span of 5 minutes. Once the degree got to 110
machine was shut off.
Now that looked like a CPU overheat.

Water cooling systems are a bit more involved that just a radiator with
a fan. My one looked like that:

![diagram](/posts.data/280-cooling-failure/00-diagram.jpg)

Here dotted line is the water flow. Fans blow the air up right into the
system case ceiling where a few small holes let it out. The system fans
(not drawn in picture) blow new air in from the right and blow it out
from the left.
In hindsight I think the radiator was positioned incorrectly and it
ought to be mounted vertically to align with the rest of air flow it the
system.

This is how radiator top looks like:

![radiator](/posts.data/280-cooling-failure/01-radiator.jpg)

And this is how it looks when I flip it to show fans (the pump is
already unscrewed):

![unscrewed pump](/posts.data/280-cooling-failure/02-unscrewed-pump.jpg)

Here I pulled the rotary part of the pump. It's a magnet in plastic
case. As I understand that way pump can be fully isolated from electric
wires and can be rotated by using just magnetic field:

![pulled rotor](/posts.data/280-cooling-failure/03-pulled-rotor.jpg)

If I screw pump back in here is how it's wiring looks like: some kind of
epoxy covers most of wiring and electronics. As a result there is not
much to do unless we are to break that layer. I was not able to find the
replacement part in online shops. Looks special.

![closer pump](/posts.data/280-cooling-failure/04-closer-pump.jpg)

Here is the heat sink: it applies to the CPU and has two tubes that come
out of it. It has the wire (probably to measure temperature) but
otherwise is passive. All the buzzing is expected to happen in the
hanging pump.

![heat sink](/posts.data/280-cooling-failure/05-heatsink.jpg)

I was a bit disappointed by this cooling system and wanted to try
something simpler and more serviceable.
After a few day of fruitlessly trying to revive the pump I plugged this
thing instead:

![air cooler](/posts.data/280-cooling-failure/06-air-cooler.jpg)

Now all the fans in the system blow the air right-to-left including the
CPU ones. `EFI` shows that CPU temperature dropped down to 30.

I can't distinguish the sound of idle system from sound fully loaded
system. Both are very quiet. We'll see how it will change in 2 years.
I wonder if I was very unlucky with this water cooler or it's a more
general pattern.
