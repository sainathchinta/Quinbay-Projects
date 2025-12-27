package com.gdn.mta.product.repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class OfflineItemRepositoryBean implements OfflineItemRepository {

  @Autowired
  private XProductFeign xProductFeign;

  @Override
  public List<OfflineItemPriceResponse> findOfflineItemByBusinessPartnerCodeAndItemSku(
      String businessPartnerCode, String itemSku) throws Exception {
    GdnRestListResponse<OfflineItemPriceResponse> response =
        xProductFeign.findOfflinePriceByMerchantCodeAndItemSku(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            businessPartnerCode, itemSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public OfflineItemResponse findOfflineItemByBusinessPartnerCodeAndMerchantSkus(
      String businessPartnerCode, List<String> merchantSkus) throws Exception {
    GdnRestSingleResponse<OfflineItemResponse> response =
        xProductFeign.getOfflineItemsByMerchantCodeAndMerchantSkus(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            businessPartnerCode, new SimpleListStringRequest(new ArrayList<>(merchantSkus)));
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public GdnBaseRestResponse updateOfflineItemPriceByItemSku(String merchantCode,
      UpdateOfflineItemPriceRequest request) throws Exception {
    GdnBaseRestResponse response = this.xProductFeign.updateOfflineItemPriceByItemSku(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), this.getRequestId(), this.getUsername(),
        merchantCode, request);
    if(!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response;
  }

  private String getRequestId() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ?
        UUID.randomUUID().toString() :
        GdnMandatoryRequestParameterUtil.getRequestId();
  }

  private String getUsername() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ?
        GdnBaseLookup.DEFAULT_USERNAME :
        GdnMandatoryRequestParameterUtil.getUsername();
  }
}
