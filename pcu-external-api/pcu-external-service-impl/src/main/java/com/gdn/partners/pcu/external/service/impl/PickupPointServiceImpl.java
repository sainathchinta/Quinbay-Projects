package com.gdn.partners.pcu.external.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.request.PickupPointDetailWebRequest;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointStockAndInBoundStatusWebResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.PickupPointFeign;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.BPService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.response.PickupPointSummaryWebResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;

import static com.gdn.partners.pcu.external.model.Constants.CODE;

/**
 * Created by govind on 12/12/2018 AD.
 */
@Service
@Slf4j
public class PickupPointServiceImpl implements PickupPointService {

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private PickupPointFeign pickupPointFeign;

  @Autowired
  private BPService bpService;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private UserPicService userPicService;

  @Autowired
  private XInventoryFeign inventoryFeign;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${max.fetch.size.stock.status.pickup.point}")
  private int maxFetchSizeForStockStatusByPPCode;


  @Override
  public void validateAndSaveDefaultPickupPoint(ProductBusinessPartnerServiceRequest request) throws Exception{
    if (CollectionUtils.isNotEmpty(request.getProductItemBusinessPartners()) && request
        .getProductItemBusinessPartners().get(0).isMarkDefaultAddress()) {
      String pickupPointCode = request.getProductItemBusinessPartners().get(0).getPickupPointId();
      this.markDefaultAddressForBusinessPartner(request.getStoreId(), request.getUsername(),
          request.getBusinessPartnerCode(), pickupPointCode);
    }
  }

  @Override
  public void validateAndSaveDefaultPickupPoint(String username, ProductCreationRequest request) throws Exception{
    if (CollectionUtils.isNotEmpty(request.getProductItemRequests()) && request.getProductItemRequests().get(0)
        .isMarkDefaultAddress()) {
      String pickupPointCode = request.getProductItemRequests().get(0).getPickupPointId();
      this.markDefaultAddressForBusinessPartner(request.getStoreId(), username,
          request.getBusinessPartnerCode(), pickupPointCode);
    }
  }

  /**
   * mark default address for pickup point
   * @param storeId
   * @param username
   * @param businessPartnerCode
   * @param pickupPointCode
   * @throws Exception
   */
  public void markDefaultAddressForBusinessPartner(String storeId, String username, String businessPartnerCode,
      String pickupPointCode) throws Exception{
    GdnBaseRestResponse
        response = pickupPointFeign.markDefaultAddress(businessPartnerCode, pickupPointCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<PickupPointSummaryWebResponse> getPickupPointSummaryFilter(String storeId, int page, int size,
      PickupPointSummaryWebRequest pickupPointSummaryWebRequest) throws Exception{
    Set<String> accessiblePickupPoints = userPicService.fetchAccessiblePickupPointCodes(null);
    if (StringUtils.isBlank(pickupPointSummaryWebRequest.getProductSku())) {
      userPicService.validateUserPicPickupPointsForBusinessPartner(
              pickupPointSummaryWebRequest.getPickupPointCodes(), null);
      Page<PickupPointOutboundResponse> pickupPointResponsePage = businessPartnerService.filterBusinessPartner(page, size,
          RequestHelper.toPickupPointFilterRequest(pickupPointSummaryWebRequest, accessiblePickupPoints,
              setWaitingDeletionForDeletePickupPoint));
      return new PageImpl<>(ResponseHelper.toPickupPointSummaryWebResponse(pickupPointResponsePage.getContent()),
          PageRequest.of(page, size), pickupPointResponsePage.getTotalElements());
    } else {
      GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> response =
          xProductFeign.getBusinessPartnerPickupPointSummary(page, size,
              RequestHelper.toPickupPointSummaryRequest(pickupPointSummaryWebRequest, accessiblePickupPoints));
      ResponseHelper.validateResponse(response);
      return new PageImpl<>(
          ResponseHelper.toPickupPointSummaryWebResponseFromBusinessPartnerPickupPoint(response.getContent()),
          PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
    }
  }

  @Override
  public List<PickupPointDetailWebResponse> fetchPickupPointDetailsByRequest(
    PickupPointDetailWebRequest pickupPointDetailWebRequest) {
    GdnRestListResponse<PickupPointDetailResponse> response =
      xProductFeign.getPickupPointDetailByCodes(
        new SimpleListStringRequest(pickupPointDetailWebRequest.getPickupPointCodes()));
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toPickupPointDetailWebResponse(response.getContent());
  }

  @Override
  public Page<PickupPointDetailWebResponse> fetchAccessiblePickupPoints(int page, int size,
      PickupPointDetailWebRequest pickupPointDetailWebRequest, String businessPartnerCode) {
    PickupPointFilterRequest request = PickupPointFilterRequest.builder()
        .businessPartnerCode(businessPartnerCode)
        .codes(new HashSet<>(pickupPointDetailWebRequest.getPickupPointCodes()))
        .name(pickupPointDetailWebRequest.getKeyword())
        .sortedBy(CODE)
        .sortDirection(Sort.Direction.DESC.name())
        .build();
    RequestHelper.setWaitingDeletionFlagForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint, request);
    GdnRestListResponse<PickupPointResponse> response =
        pickupPointFeign.fetchAccessiblePickupPoints(page, size, request);
    ResponseHelper.validateResponse(response);
    List<PickupPointDetailWebResponse> pickupPoints = response.getContent().stream()
        .map(pickupPoint -> PickupPointDetailWebResponse.builder()
            .pickupPointCode(pickupPoint.getCode())
            .pickupPointName(pickupPoint.getName()).build()).collect(Collectors.toList());
    return new PageImpl<>(pickupPoints, buildPageableFromPageMetadata(response.getPageMetaData()),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<PickupPointStockAndInBoundStatusWebResponse> fetchStockAndInBoundStatusByProductSkuAndPPCode(
    String businessPartnerCode, String storeId,
    PickupPointDetailWebRequest pickupPointDetailWebRequest, String productSku, int page,
    int size) {

    List<PickupPointStockAndInBoundStatusWebResponse> results = new ArrayList<>();
    List<String> pickupPointCodes = pickupPointDetailWebRequest.getPickupPointCodes();
    int limit = Math.min(pickupPointCodes.size(), maxFetchSizeForStockStatusByPPCode);
    if (limit < pickupPointCodes.size()) {
      log.info(
        "Reducing fetch for #StockAndInBoundStatusByProductSkuAndPPCode for {} from {} to " + "{}",
        productSku, pickupPointCodes.size(), maxFetchSizeForStockStatusByPPCode);
    }
    fetchResponseFromInventoryForStockStatus(storeId, productSku, pickupPointCodes, limit, results);
    return new PageImpl<>(results, PageRequest.of(page, size), results.size());
  }

  private void fetchResponseFromInventoryForStockStatus(String storeId, String productSku, List<String> pickupPointCodes,
    int limit, List<PickupPointStockAndInBoundStatusWebResponse> results) {
    pickupPointCodes.stream().limit(limit).forEach(pickupPointCode -> {
      try {
        GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> response =
          inventoryFeign.getStockAvailabilityByL3AndPickupPoint(storeId,
            mandatoryParameterHelper.getRequestId(), mandatoryParameterHelper.getChannelId(),
            mandatoryParameterHelper.getClientId(), mandatoryParameterHelper.getUsername(),
            productSku, pickupPointCode);

        ResponseHelper.validateResponse(response);
        results.add(ResponseHelper.toL3AndPickupPointStockAvailabilityResponse(response));
      } catch (ClientException e) {
        log.error("Skipping failed API call for pickup point and productSku {}: {}",
          productSku.concat(Constants.HYPHEN).concat(pickupPointCode), e.getMessage());
      }
    });
  }


  private Pageable buildPageableFromPageMetadata(PageMetaData pageMetaData) {
    if (pageMetaData.getPageSize() <= 0) {
      return Pageable.unpaged();
    }
    return PageRequest.of((int)pageMetaData.getPageNumber(), (int) pageMetaData.getPageSize());
  }
}
