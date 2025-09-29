import 'package:flutter/material.dart';

/// Optimized image widget with caching and lazy loading
class OptimizedImage extends StatelessWidget {
  final String? imageUrl;
  final String? assetPath;
  final double? width;
  final double? height;
  final BoxFit fit;
  final Widget? placeholder;
  final Widget? errorWidget;
  final bool enableMemoryCache;
  final bool enableDiskCache;
  final Duration? fadeInDuration;
  final Alignment alignment;
  
  const OptimizedImage({
    super.key,
    this.imageUrl,
    this.assetPath,
    this.width,
    this.height,
    this.fit = BoxFit.cover,
    this.placeholder,
    this.errorWidget,
    this.enableMemoryCache = true,
    this.enableDiskCache = true,
    this.fadeInDuration,
    this.alignment = Alignment.center,
  }) : assert(imageUrl != null || assetPath != null, 'Either imageUrl or assetPath must be provided');

  @override
  Widget build(BuildContext context) {
    // Asset image
    if (assetPath != null) {
      return Image.asset(
        assetPath!,
        width: width,
        height: height,
        fit: fit,
        alignment: alignment,
        errorBuilder: (context, error, stackTrace) => 
            errorWidget ?? _buildDefaultErrorWidget(),
        frameBuilder: (context, child, frame, wasSynchronouslyLoaded) {
          if (wasSynchronouslyLoaded) return child;
          return AnimatedOpacity(
            opacity: frame == null ? 0 : 1,
            duration: fadeInDuration ?? const Duration(milliseconds: 300),
            child: child,
          );
        },
      );
    }
    
    // Network image with built-in caching
    if (imageUrl != null && imageUrl!.isNotEmpty) {
      return Image.network(
        imageUrl!,
        width: width,
        height: height,
        fit: fit,
        alignment: alignment,
        loadingBuilder: (context, child, loadingProgress) {
          if (loadingProgress == null) {
            return AnimatedOpacity(
              opacity: 1,
              duration: fadeInDuration ?? const Duration(milliseconds: 300),
              child: child,
            );
          }
          return placeholder ?? _buildDefaultPlaceholder();
        },
        errorBuilder: (context, error, stackTrace) => 
            errorWidget ?? _buildDefaultErrorWidget(),
        frameBuilder: (context, child, frame, wasSynchronouslyLoaded) {
          if (wasSynchronouslyLoaded) return child;
          return AnimatedOpacity(
            opacity: frame == null ? 0 : 1,
            duration: fadeInDuration ?? const Duration(milliseconds: 300),
            child: child,
          );
        },
      );
    }
    
    return errorWidget ?? _buildDefaultErrorWidget();
  }

  Widget _buildDefaultPlaceholder() {
    return Container(
      width: width,
      height: height,
      color: Colors.grey[300],
      child: const Center(
        child: CircularProgressIndicator(strokeWidth: 2),
      ),
    );
  }

  Widget _buildDefaultErrorWidget() {
    return Container(
      width: width,
      height: height,
      color: Colors.grey[200],
      child: Icon(
        Icons.error_outline,
        color: Colors.grey[400],
        size: 24,
      ),
    );
  }
}

/// Memory-efficient image cache manager
class ImageCacheManager {
  static final ImageCacheManager _instance = ImageCacheManager._internal();
  factory ImageCacheManager() => _instance;
  ImageCacheManager._internal();

  /// Clear memory cache
  void clearMemoryCache() {
    imageCache.clear();
    imageCache.clearLiveImages();
  }

  /// Set memory cache limits
  void configureMemoryCache({
    int? maxCacheSize,
    int? maxCacheObjects,
  }) {
    if (maxCacheSize != null) {
      imageCache.maximumSizeBytes = maxCacheSize;
    }
    if (maxCacheObjects != null) {
      imageCache.maximumSize = maxCacheObjects;
    }
  }

  /// Get cache status
  Map<String, dynamic> getCacheStatus() {
    return {
      'currentSize': imageCache.currentSizeBytes,
      'maxSize': imageCache.maximumSizeBytes,
      'currentObjects': imageCache.currentSize,
      'maxObjects': imageCache.maximumSize,
      'liveImageCount': imageCache.liveImageCount,
      'pendingImageCount': imageCache.pendingImageCount,
    };
  }
}