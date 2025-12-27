package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class PristineMasterDataResponse extends BaseResponse{

  private static final long serialVersionUID = -285288528L;

  private String pristineMasterId;

  public PristineMasterDataResponse() {
  }

  public String getPristineMasterId() {
    return pristineMasterId;
  }

  public void setPristineMasterId(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;
  }

  public PristineMasterDataResponse(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;

  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof PristineMasterDataResponse)) return false;
    PristineMasterDataResponse that = (PristineMasterDataResponse) o;
    return GdnObjects.equals(pristineMasterId, that.pristineMasterId);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(pristineMasterId);
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("pristineMasterId", pristineMasterId)
        .toString();
  }
}
