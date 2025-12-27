package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gda.mta.product.dto.ImageInformationRequest;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

public class XGPImageInfoDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 4592167772303537414L;
  private String productCode;
  private String storeId;
  private String clientId;
  private String username;
  private List<ImageInformationRequest> imageInformationRequests;

  public XGPImageInfoDomainEvent(String productCode, String storeId, String username, String clientId,
      List<ImageInformationRequest> imageInformationRequests) {
    this.productCode = productCode;
    this.clientId = clientId;
    this.storeId = storeId;
    this.username = username;
    this.imageInformationRequests = imageInformationRequests;
  }

  public XGPImageInfoDomainEvent() {

  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public List<ImageInformationRequest> getImageInformationRequests() {
    return imageInformationRequests;
  }

  public void setImageInformationRequests(List<ImageInformationRequest> imageInformationRequests) {
    this.imageInformationRequests = imageInformationRequests;
  }


  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("storeId", storeId)
        .append("username", username).append("clientId", clientId)
        .append("imageInformationRequests", imageInformationRequests).toString();
  }
}
