package com.gdn.micro.graphics.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;

import org.im4java.process.OutputConsumer;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public class IdentifyImageResult implements OutputConsumer, Cloneable {
  private boolean image = false;
  private Integer width;
  private Integer height;
  private boolean containResolution = false;
  private Integer resolutionWidth;
  private Integer resolutionHeight;
  private Integer quality;
  private String imageType;
  private static final List<String> SUPPORTED_IMAGE_TYPES = Arrays.asList("PNG", "JPEG", "WEBP");

  @Override
  public void consumeOutput(InputStream inputStream) throws IOException {
    try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
      //we'll just take first line
      String data = br.readLine();

      //for now, we will only consider PNG and JPG as image
      //format template
      //%m,%Q,%w,%h
      try {
        if(!data.contains(",")) {
          image = false;
          return;
        }
        String[] imageData = data.split(",");

        imageType = imageData[0];
        if (!SUPPORTED_IMAGE_TYPES.contains(imageType)) {
          image = false;
          return;
        }
        quality = Integer.valueOf(imageData[1]);
        width = Integer.valueOf(imageData[2]);
        height = Integer.valueOf(imageData[3]);
        image = true;
      }catch(IndexOutOfBoundsException ioe){
        image = false;
      }

    }
  }

  public Integer getHeight() {
    return height;
  }

  public Integer getQuality() {
    return quality == null ? 0 : quality;
  }

  public Integer getResolutionHeight() {
    return resolutionHeight == null ? 0 :resolutionHeight;
  }

  public Integer getResolutionWidth() {
    return resolutionWidth == null ? 0 : resolutionWidth;
  }

  public Integer getWidth() {
    return width == null? 0 : width;
  }

  public boolean isContainResolution() {
    return containResolution;
  }

  public boolean isImage() {
    return image;
  }

  public String getImageType(){ return imageType;}
}
