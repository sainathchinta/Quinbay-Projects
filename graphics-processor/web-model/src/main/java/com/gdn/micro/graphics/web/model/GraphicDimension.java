package com.gdn.micro.graphics.web.model;

/**
 * @author Yudhi K. Surtan
 *
 */
public class GraphicDimension {

  private Integer width;
  private Integer height;

  public GraphicDimension() {
    // nothing here
  }

  public GraphicDimension(Integer width, Integer height) {
    this.width = width;
    this.height = height;
  }

  public Integer getHeight() {
    return height;
  }

  public Integer getWidth() {
    return width;
  }

  public void setHeight(Integer height) {
    this.height = height;
  }

  public void setWidth(Integer width) {
    this.width = width;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("GraphicDimension [width=").append(width).append(", height=").append(height)
    .append("]");
    return builder.toString();
  }


}
