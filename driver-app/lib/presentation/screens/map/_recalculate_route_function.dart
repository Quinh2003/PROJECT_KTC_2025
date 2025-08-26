// Function to manually recalculate route from current camera position
Future<void> _recalculateRouteFromCurrentView() async {
  if (_mapboxMap == null || !mounted) return;

  try {
    setState(() {
      _isRouteDataLoading = true;
    });

    final cameraState = await _mapboxMap!.getCameraState();
    final centerCoordinates = cameraState.center.coordinates;
    final centerPos =
        Position(centerCoordinates[0]?.toDouble() ?? 0.0, centerCoordinates[1]?.toDouble() ?? 0.0);

    await _loadRouteDataForNewArea(centerPos);
      
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Route recalculated for this area'),
        duration: Duration(seconds: 2),
      ),
    );
  } catch (e) {
    print("❌ Error recalculating route: $e");
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Failed to recalculate route: $e'),
        duration: Duration(seconds: 2),
        backgroundColor: Colors.red,
      ),
    );
  } finally {
    if (mounted) {
      setState(() {
        _isRouteDataLoading = false;
      });
    }
  }
}
