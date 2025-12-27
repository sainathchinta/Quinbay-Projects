package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.BusinessPartnerPickupPointRepository;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.util.ResponseHelper;

@Service
public class BusinessPartnerPickupPointServiceImpl implements BusinessPartnerPickupPointService {

  @Autowired
  private BusinessPartnerPickupPointRepository businessPartnerPickupPointRepository;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Override
  public BusinessPartnerPickupPoint saveBusinessPartnerPickupPoint(
      BusinessPartnerPickupPoint businessPartnerPickupPoint) {
    return businessPartnerPickupPointRepository.save(businessPartnerPickupPoint);
  }

  @Override
  public BusinessPartnerPickupPoint getBusinessPartnerPickupPoint(String storeId, String businessPartnerCode,
      String pickupPointCode) {
    return businessPartnerPickupPointRepository.findByBusinessPartnerCodeAndCode(businessPartnerCode, pickupPointCode);
  }

  @Override
  public List<BusinessPartnerPickupPoint> getBusinessPartnerPickupPointByPickupPointCodes(String storeId,
      List<String> pickupPointCodes) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), "PickupPoints cannot be empty");
    return businessPartnerPickupPointRepository.findByCodeIn(
      pickupPointCodes.stream().distinct().collect(Collectors.toList()));
  }

  @Override
  public List<BusinessPartnerPickupPoint> getBusinessPartnerPickupPointByBusinessPartnerCode(
      String businessPartnerCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    return businessPartnerPickupPointRepository.findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(
        businessPartnerCode);
  }

  @Override
  public Page<BusinessPartnerPickupPointResponse> getBusinessPartnerPickupPointSummary(String storeId, int page,
      int size, PickupPointSummaryRequest pickupPointSummaryRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointSummaryRequest.getBusinessPartnerCode()),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointSummaryRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_EMPTY);

    Product product =
        productCacheableService.findProductByStoreIdAndProductSku(storeId, pickupPointSummaryRequest.getProductSku());

    Set<String> pickupPointCodes = Optional.ofNullable(product).map(Product::getPickupPointCodes).orElse(new HashSet<>());

    if(CollectionUtils.isNotEmpty(pickupPointSummaryRequest.getCodes())) {
      pickupPointCodes.retainAll(pickupPointSummaryRequest.getCodes());
    }

    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList =
        businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(storeId,
            pickupPointSummaryRequest.getBusinessPartnerCode(), pickupPointSummaryRequest.getKeyword(),
            pickupPointSummaryRequest.getCncActivated(),pickupPointSummaryRequest.getFbbActivated(),
            pickupPointSummaryRequest.getCodes());

    if (CollectionUtils.isNotEmpty(businessPartnerPickupPointList)) {
      long recordCount = businessPartnerPickupPointList.size();
      List<BusinessPartnerPickupPoint> selectedBusinessPartnerPickupPointList =
        businessPartnerPickupPointList.stream().filter(
          businessPartnerPickupPoint -> pickupPointCodes.contains(
            businessPartnerPickupPoint.getCode())).collect(Collectors.toList());
      businessPartnerPickupPointList.removeAll(selectedBusinessPartnerPickupPointList);
      businessPartnerPickupPointList.sort(
        Comparator.comparing(BusinessPartnerPickupPoint::getCode));
      selectedBusinessPartnerPickupPointList.sort(
        Comparator.comparing(BusinessPartnerPickupPoint::getCode));
      selectedBusinessPartnerPickupPointList.addAll(businessPartnerPickupPointList);
      List<BusinessPartnerPickupPoint> finalList =
        selectedBusinessPartnerPickupPointList.stream().skip(page * size).limit(size)
          .collect(Collectors.toList());
      return new PageImpl<>(ResponseHelper.toBusinessPartnerPickupPointResponse(finalList),
        PageRequest.of(page, size), recordCount);
    }
    return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
  }

  @Override
  public List<String> getCncActivatedPickupPointCodes(Set<String> pickupPointCodes) {
    GdnPreconditions.checkArgument(!pickupPointCodes.isEmpty(), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    List<String> cncActivatedPickupPointCodes = new ArrayList<>();
    for (String pickupPointCode : pickupPointCodes) {
      BusinessPartnerPickupPoint businessPartnerPickupPoint =
          businessPartnerPickupPointRepository.findByCode(pickupPointCode);
      if (Objects.nonNull(businessPartnerPickupPoint) && businessPartnerPickupPoint.isCncActivated()) {
        cncActivatedPickupPointCodes.add(businessPartnerPickupPoint.getCode());
      }
    }
    return cncActivatedPickupPointCodes;
  }

  @Override
  public List<String> getFbbActivatedPickupPointCodes(Set<String> pickupPointCodes) {
    GdnPreconditions.checkArgument(!pickupPointCodes.isEmpty(), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    List<String> fbbActivatedPickupPointCodes = new ArrayList<>();
    for (String pickupPointCode : pickupPointCodes) {
      BusinessPartnerPickupPoint businessPartnerPickupPoint =
          businessPartnerPickupPointRepository.findByCode(pickupPointCode);
      if (Objects.nonNull(businessPartnerPickupPoint) && businessPartnerPickupPoint.isFbbActivated()) {
        fbbActivatedPickupPointCodes.add(businessPartnerPickupPoint.getCode());
      }
    }
    return fbbActivatedPickupPointCodes;
  }
}
