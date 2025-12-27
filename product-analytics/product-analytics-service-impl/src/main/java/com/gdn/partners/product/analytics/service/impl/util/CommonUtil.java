package com.gdn.partners.product.analytics.service.impl.util;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.SellerResponse;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.Objects;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommonUtil {

  public static AutoApprovedListWebResponse mapToAutoApprovedWebResponse(
    AutoApprovedProducts product) {
    return AutoApprovedListWebResponse.builder().productCode(product.getProductCode())
      .productName(product.getProductName()).categoryCode(product.getCategoryCode())
      .categoryName(product.getCategoryName()).commissionType(product.getCommissionType())
      .addedDate(fetchValidDate(product.getAddedDate())).assignedTo(product.getAssignedTo())
      .assignedDate(fetchValidDate(product.getAssignedDate())).reason(product.getReason())
      .sourceEn(product.getSourceEn()).sourceId(product.getSourceId())
      .b2bActivated(product.isB2bActivated()).seller(
        SellerResponse.builder().sellerCode(product.getSellerCode())
          .sellerName(product.getSellerName()).sellerBadge(product.getSellerBadge())
          .official(product.isOfficialSeller()).internationalSeller(product.isInternationalSeller())
          .build()).build();
  }

  public static Long fetchValidDate(Date inputDate) {
    if (Objects.isNull(inputDate)) {
      return null;
    }
    return inputDate.getTime();
  }
}
