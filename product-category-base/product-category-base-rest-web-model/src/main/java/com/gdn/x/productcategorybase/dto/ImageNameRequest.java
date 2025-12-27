package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;
import java.util.List;

public class ImageNameRequest implements Serializable {

  public ImageNameRequest(List<UpdateImageDTO> productImage, List<UpdateImageDTO> productItemImage) {
    super();
    this.productImage = productImage;
    this.productItemImage = productItemImage;
  }

  private static final long serialVersionUID = 9189304332237219179L;
 
  private List<UpdateImageDTO> productImage;
  private List<UpdateImageDTO> productItemImage;
  public ImageNameRequest() {
    
  }
 
  public List<UpdateImageDTO> getProductItemImage() {
    return productItemImage;
  }
  public void setProductItemImage(List<UpdateImageDTO> productItemImage) {
    this.productItemImage = productItemImage;
  }
  
  public List<UpdateImageDTO> getProductImage() {
    return productImage;
  }

  public void setProductImage(List<UpdateImageDTO> productImage) {
    this.productImage = productImage;
  }

  @Override
  public String toString() {
    return String.format(
        "ImageNameRequest [productImage=%s, productItemImage=%s, getProductItemImage()=%s]",
        productImage, productItemImage, getProductItemImage());
  }
  
}
