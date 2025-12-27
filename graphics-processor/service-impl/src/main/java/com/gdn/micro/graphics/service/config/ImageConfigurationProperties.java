package com.gdn.micro.graphics.service.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import lombok.Data;

/**
 * Configuration properties for image processing settings
 */
@Data
@Component
@ConfigurationProperties(prefix = "image")
public class ImageConfigurationProperties {

    private int fullDpi;
    private int fullQuality;
    private int fullWidth;
    private int fullHeight;

    private int mediumDpi;
    private int mediumQuality;
    private int mediumWidth;
    private int mediumHeight;

    private int thumbnailDpi;
    private int thumbnailQuality;
    private int thumbnailWidth;
    private int thumbnailHeight;

    // WebP conversion settings
    private int webpDpi;
    private int webpQuality;
} 