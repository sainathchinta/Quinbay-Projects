package com.gdn.mta.bulk.repository;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.CacheKeys;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.XBPFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;

import java.util.List;
import java.util.Objects;

@Repository
public class BusinessPartnerRepositoryBean implements BusinessPartnerRepository {

  @Autowired
  private XBPFeign xbpFeign;

  @Autowired
  private ApplicationContext applicationContext;

  @Value("${use.cache.for.business.partner.response}")
  private boolean useCacheForBusinessPartnerResponse;

  @Override
  public ProfileResponse filterByBusinessPartnerCodeV2(String storeId, String businessPartnerCode) throws Exception {
    if (useCacheForBusinessPartnerResponse) {
      return applicationContext.getBean(BusinessPartnerRepositoryBean.class)
          .filterByBusinessPartnerCodeV2Cache(storeId, businessPartnerCode);
    }
    return getProfileResponse(storeId, businessPartnerCode);
  }

  private ProfileResponse getProfileResponse(String storeId, String businessPartnerCode) throws ApplicationException {
    GdnRestSingleResponse<ProfileResponse> response =
        xbpFeign.filterByBusinessPartnerCode(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, businessPartnerCode);
    this.validateResponse(response);
    return response.getValue();
  }

  @Override
  @Cacheable(cacheManager = CacheKeys.CAFFEINE_CACHE_MANAGER, value = CacheKeys.PROFILE_RESPONSE,
      key = "#businessPartnerCode", unless = "#result == null")
  public ProfileResponse filterByBusinessPartnerCodeV2Cache(String storeId, String businessPartnerCode)
      throws Exception {
    return getProfileResponse(storeId, businessPartnerCode);
  }

  @Override
  public Page<PickupPointResponse> filterBusinessPartnerPickupPointV2(int page, int size,
    PickupPointFilterRequest request) throws ApplicationException {
    GdnRestListResponse<PickupPointResponse> response = xbpFeign
      .getPickupPointList(Constant.STORE_ID, Constant.USER_NAME, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, page, size, request);
    this.validateResponse(response);
    return new PageImpl<>(response.getContent(),  PageRequest.of(page, size),
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<ProfileResponse> filterByBusinessPartnerCodeList(
    BusinessPartnerFilterRequest businessPartnerFilterRequest, int page, int size)
    throws Exception {
    GdnRestListResponse<ProfileResponse> response =
      xbpFeign.getBusinessPartnerDetailsByList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), page, size, businessPartnerFilterRequest);
    this.validateResponse(response);
    return response.getContent();
  }

  private void validateResponse(GdnBaseRestResponse response) throws ApplicationException{
    if (Objects.isNull(response)) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
        "Null response from XBP");
    }
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(StringUtils.isEmpty(response
          .getErrorCode()) ? ErrorCategory.UNSPECIFIED.toString() : response.getErrorCode()),
          "XBP error: " + response.getErrorMessage());
    }
  }

}
