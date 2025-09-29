import 'package:url_launcher/url_launcher.dart';
import 'package:flutter/foundation.dart';

class UrlLauncherFrave {

  Future<bool> openMapLaunch(String latitude, String longitude) async {
    var url = 'google.navigation:q=$latitude,$longitude&mode=d';
    var urlGoogleMap =  'https://www.google.com/maps/search/?api=1&query=$latitude,$longitude';

    try {
      bool isLaunched = await launchUrl(Uri.parse(url));
      if(!isLaunched) {
        isLaunched = await launchUrl(Uri.parse(urlGoogleMap));
      }
      return isLaunched;
    } catch (e) {
      debugPrint('Error opening map: $e');
      return false;
    }
  }

  Future<bool> openMap(String address) async {
    final encodedAddress = Uri.encodeComponent(address);
    final url = 'https://www.google.com/maps/search/?api=1&query=$encodedAddress';
    
    try {
      bool isLaunched = await launchUrl(
        Uri.parse(url),
        mode: LaunchMode.externalApplication
      );
      return isLaunched;
    } catch (e) {
      debugPrint('Error opening map: $e');
      return false;
    }
  }
  
  Future<bool> launchURL(String url) async {
    try {
      bool isLaunched = await launchUrl(
        Uri.parse(url),
        mode: LaunchMode.externalApplication,
      );
      return isLaunched;
    } catch (e) {
      debugPrint('Error launching URL: $e');
      return false;
    }
  }
  
  Future<bool> openMapWithWaypoints({
    required String origin,
    required String destination,
    List<String> waypoints = const [],
  }) async {
    try {
      // Encode addresses
      final encodedOrigin = Uri.encodeComponent(origin);
      final encodedDestination = Uri.encodeComponent(destination);
      
      // Build Google Maps URL with waypoints
      String url = 'https://www.google.com/maps/dir/?api=1'
          '&origin=$encodedOrigin'
          '&destination=$encodedDestination';
      
      // Add waypoints if provided
      if (waypoints.isNotEmpty) {
        final encodedWaypoints = waypoints
            .map((waypoint) => Uri.encodeComponent(waypoint))
            .join('|');
        url += '&waypoints=$encodedWaypoints';
      }
      
      // Set travel mode to driving
      url += '&travelmode=driving&dir_action=navigate';
      
      debugPrint('Opening map with waypoints: $url');
      return await launchUrl(
        Uri.parse(url),
        mode: LaunchMode.externalApplication,
      );
    } catch (e) {
      debugPrint('Error opening map with waypoints: $e');
      return false;
    }
  }

  Future<bool> makePhoneCall(String url) async {
    try {
      if (await canLaunchUrl(Uri.parse(url))) {
        return await launchUrl(
          Uri.parse(url),
          mode: LaunchMode.externalApplication
        );
      } else {
        debugPrint('Could not launch $url');
        return false;
      }
    } catch (e) {
      debugPrint('Error making phone call: $e');
      return false;
    }
  }
}

final urlLauncherFrave = UrlLauncherFrave();
