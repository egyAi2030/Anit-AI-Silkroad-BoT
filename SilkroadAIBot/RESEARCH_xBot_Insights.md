# xBot Technical Insights Research

Findings from `resources/resources/Tools/xBot-WinForms-master/xBot-WinForms-master` to inform SilkroadAIBot development.

## 1. Coordinate Mapping (`SRCoord.cs`)

xBot uses a global coordinate system (`PosX`, `PosY`) derived from local region coordinates (`X`, `Y`) and sectors (`xSector`, `ySector`).

- **Local to Global**:

  ```csharp
  PosX = (xSector - 135) * 192 + X / 10;
  PosY = (ySector - 92) * 192 + Y / 10;
  ```

- **Dungeon Handling**: Dungeons (`Region > 32767`) use a fixed offset:

  ```csharp
  PosX = 128 * 192 + X / 10;
  PosY = 128 * 192 + Y / 10;
  ```

*Implementation Note*: SilkroadAIBot has adopted this mapping in `SRCoord.cs` to enable distance calculations across regions.

## 2. Training Area Logic (`Bot.IA.cs`)

xBot implements a radius-based training area with two primary movement modes:

- **Walk to Center**: If the bot is far from the center but within radius, it optionally walks back to the exact center point.
- **Random Walk**: If no mobs are present, it selects a random point within the radius:

  ```csharp
  int random = rand.Next(-radius, radius);
  newPosition = new SRCoord(center.PosX + random, center.PosY + random);
  ```

- **Boundary Check**:

  ```csharp
  if (myPosition.DistanceTo(trainingPosition) > trainingRadius + 50) {
      // Return to center or stop
  }
  ```

## 3. Bot Loop Structure

- **State Check**: The loop constantly checks "Where am I?" (Town, Script, or Training Area).
- **Prioritization**:
  1. Town Scripts (Maintenance)
  2. Training Area (Combat)
  3. Script Navigation (Getting to Area)
- **Wait Handles**: uses `WaitHandle.WaitAny` to wait for movement completion, mob spawns, or buff removals simultaneously.

## 4. Advanced Features (Found but not yet implemented)

- **Weapon Swapping**: Logic to swap weapons based on skill requirements.
- **Buff Loop**: Separate logic to maintain status effects.
- **Script Support**: A proprietary script format for complex pathing.
