package com.gdn.x.product.rest.web.model.request;

import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
@JsonIgnoreProperties(ignoreUnknown = true)
public class GetShippingTypeRequest extends BaseRequest {

  private static final long serialVersionUID = -5284526026609772382L;

  private List<String> categories;
  private String logisticProviderId;

  public GetShippingTypeRequest() {}

  public GetShippingTypeRequest(List<String> categories, String shipping) {
    this.categories = categories;
    this.logisticProviderId = shipping;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  public List<String> getCategories() {
    return this.categories;
  }

  public String getLogisticProviderId() {
    return this.logisticProviderId;
  }


  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  public void setCategories(List<String> categories) {
    this.categories = categories;
  }

  public void setLogisticProviderId(String shipping) {
    this.logisticProviderId = shipping;
  }

  @Override
  public String toString() {
    return String.format("GetShippingTypeRequest [categories=%s, shipping=%s, toString()=%s]",
        this.categories, this.logisticProviderId, super.toString());
  }


}
