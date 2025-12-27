package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

import java.util.Set;
@JsonIgnoreProperties(ignoreUnknown = true)
public class GetProductInfoRequest extends BaseRequest {

  private static final long serialVersionUID = -4913092751269499707L;

  private Set<String> itemSkus;

  private boolean fullFetch;

  private boolean pristine;

  private boolean needWholesaleData;

  private boolean combineOthersBundlings;

  private boolean off2On;

  public GetProductInfoRequest() {
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Set<String> getItemSkus() {
    return itemSkus;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isCombineOthersBundlings() {
    return combineOthersBundlings;
  }

  public boolean isFullFetch() {
    return fullFetch;
  }

  public boolean isNeedWholesaleData() {
    return needWholesaleData;
  }

  public boolean isPristine() {
    return pristine;
  }

  public boolean isOff2On() {
    return off2On;
  }

  public void setCombineOthersBundlings(boolean combineOthersBundlings) {
    this.combineOthersBundlings = combineOthersBundlings;
  }

  public void setFullFetch(boolean fullFetch) {
    this.fullFetch = fullFetch;
  }

  public void setItemSkus(Set<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  public void setNeedWholesaleData(boolean needWholesaleData) {
    this.needWholesaleData = needWholesaleData;
  }

  public void setPristine(boolean pristine) {
    this.pristine = pristine;
  }

  public void setOff2On(boolean off2On) {
    this.off2On = off2On;
  }

  @Override
  public String toString() {
    return String.format(
        "GetProductInfoRequest [itemSkus=%s, fullFetch=%s, pristine=%s, needWholesaleData=%s, "
            + "combineOthersBundlings=%s, off2On=%s"
            + "toString()=%s]", itemSkus, fullFetch, pristine, needWholesaleData, combineOthersBundlings, off2On,
        super.toString());
  }
}
