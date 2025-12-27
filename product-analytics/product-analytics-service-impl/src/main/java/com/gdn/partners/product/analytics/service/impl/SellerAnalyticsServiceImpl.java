package com.gdn.partners.product.analytics.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.analytics.model.ErrorMessages;
import com.gdn.partners.product.analytics.repository.SellerAnalyticsRepository;
import com.gdn.partners.product.analytics.service.SellerAnalyticsService;
import com.gdn.partners.product.analytics.service.impl.helper.ResponseHelper;
import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
public class SellerAnalyticsServiceImpl implements SellerAnalyticsService {

  @Autowired
  private SellerAnalyticsRepository sellerAnalyticsRepository;

  @Override
  public SellerAnalyticsResponse findSellerAnalyticsDetailByStoreIdAndSellerCode(String storeId,
      String sellerCode) {
    GdnPreconditions.checkArgument(Objects.nonNull(sellerCode),
        ErrorMessages.SELLER_CODE_MUST_NOT_BE_BLANK);
    return ResponseHelper.toSellerAnalyticsResponse(
        sellerAnalyticsRepository.findByStoreIdAndSellerCode(storeId, sellerCode));
  }
}
