package com.gdn.partners.pcu.external.web.model.response;

import com.gdn.fbb.core.constant.ProductConsignmentStatus;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum ConsignmentStatusWeb {
  DONE, REJECTED, CANCELLED, IN_PROGRESS;

  public static ConsignmentStatusWeb convertToConsignmentStatusWeb(
    ProductConsignmentStatus consignmentStatus) {
    switch (consignmentStatus) {
      case DONE:
        return ConsignmentStatusWeb.DONE;
      case REJECTED:
        return ConsignmentStatusWeb.REJECTED;
      case CANCELLED:
        return ConsignmentStatusWeb.CANCELLED;
      case IN_PROGRESS:
        return ConsignmentStatusWeb.IN_PROGRESS;
    }
    return null;
  }

  public static Integer getStatusPriorityForLisitng(ConsignmentStatusWeb status) {
    switch (status) {
      case IN_PROGRESS:
        return 0;
      case DONE:
        return 1;
      case REJECTED:
        return 2;
      case CANCELLED:
        return 3;
    }
    return Integer.MAX_VALUE;
  }
}
