package com.gdn.micro.graphics.web.model;

import java.io.Serializable;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public class CustomGraphicsSettings implements Serializable {
  private static final long serialVersionUID = 1698610380534436350L;
  private int dpi = 72;
  private double quality = 75;
  private GraphicDimension dimession;

  public CustomGraphicsSettings() {
    // nothing to do here
  }

  public CustomGraphicsSettings(int dpi, double quality, GraphicDimension dimession) {
    this.dpi = dpi;
    this.quality = quality;
    this.dimession = dimession;
  }

  public CustomGraphicsSettings(Integer width, Integer height) {
    this.dimession = new GraphicDimension(width, height);
  }

  public GraphicDimension getDimession() {
    return dimession;
  }

  public int getDpi() {
    return this.dpi;
  }

  public Integer getHeight() {
    return dimession.getHeight();
  }

  public double getQuality() {
    return this.quality;
  }

  public Integer getWidth() {
    return dimession.getWidth();
  }

  public void setDimession(GraphicDimension dimession) {
    this.dimession = dimession;
  }

  public void setDpi(int dpi) {
    this.dpi = dpi;
  }

  public void setHeight(Integer height) {
    this.dimession.setHeight(height);
  }

  public void setQuality(double quality) {
    this.quality = quality;
  }

  public void setWidth(Integer width) {
    this.dimession.setWidth(width);
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("CustomGraphicsSettings [dpi=").append(dpi).append(", quality=").append(quality)
    .append(", dimession=").append(dimession).append("]");
    return builder.toString();
  }

}
