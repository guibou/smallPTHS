import subprocess

nbImages = 240 # 10s
timeDeltaMin = 0
timeDeltaMax = 4

def lerp(a, b, t):
    return a * t + b * (1 - t)

for i in range(nbImages):
    print(i)
    timeDelta = lerp(timeDeltaMin, timeDeltaMax, i / nbImages)

    subprocess.run([
        "./dist-newstyle/build/x86_64-linux/ghc-8.6.5/smallpths-0.1.0.0/x/ReallySmallPTHS/build/ReallySmallPTHS/ReallySmallPTHS",
        "--width", str(640),
        "--height", str(480),
        "--samples", str(4),
        "--timeDelta", str(timeDelta),
        "--outputFile", f"animation_{i:04d}.ppm",
        ], check=True)
