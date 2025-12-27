package com.gdn.micro.graphics.model;

import java.io.Serializable;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public class CustomGraphicsSettings implements Serializable {
  private static final long serialVersionUID = 1698610380534436350L;
  private int dpi = 72;
  private int quality = 75;
  private GraphicDimension dimension;

  public CustomGraphicsSettings() {
    // nothing to do here
  }

  public CustomGraphicsSettings(int dpi, int quality, GraphicDimension dimension) {
    this.dpi = dpi;
    this.quality = quality;
    this.dimension = dimension;
  }

  public GraphicDimension getDimession() {
    return dimension;
  }

  public GraphicDimension getDimension() {
    return dimension;
  }

  public int getDpi() {
    return this.dpi;
  }

  public Integer getHeight() {
    return dimension.getHeight();
  }

  public double getQuality() {
    return this.quality;
  }

  public Integer getWidth() {
    return dimension.getWidth();
  }

  public void setDimession(GraphicDimension dimension) {
    this.dimension = dimension;
  }

  public void setDimension(GraphicDimension dimension) {
    this.dimension = dimension;
  }

  public void setDpi(int dpi) {
    this.dpi = dpi;
  }

  public void setHeight(Integer height) {
    this.dimension.setHeight(height);
  }

  public void setQuality(int quality) {
    this.quality = quality;
  }

  public void setWidth(Integer width) {
    this.dimension.setWidth(width);
  }

  @Override
  public String toString() {
    return "CustomGraphicsSettings [dpi=" + dpi + ", quality=" + quality + ", dimession=" + dimension + "]";
  }
}
