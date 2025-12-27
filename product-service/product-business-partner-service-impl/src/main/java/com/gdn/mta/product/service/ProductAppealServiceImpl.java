package com.gdn.mta.product.service;

import com.gda.mta.product.dto.AppealProductConfigResponse;
import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.ProductBusinessPartnerCounterRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductAppealServiceImpl implements ProductAppealService {

  private final ProductBusinessPartnerCounterRepository productBusinessPartnerCounterRepository;
  private final ProductSystemParameterServiceImpl productSystemParameterService;
  private final ProductDistributionTaskRepository productDistributionTaskRepository;
  private final ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Override
  @Transactional(rollbackFor = Exception.class)
  public AppealProductResponse updateAppealProductForInProgressProducts(String storeId,
    AppealProductRequest appealProductRequest) throws ApplicationException {
    Pair<Integer, ProductBusinessPartnerCounter> appealProductLimitCounterPair =
        fetchThresholdAndCounterForAppealProduct(storeId,
            appealProductRequest.getBusinessPartnerCode());
    int appealProductLimit = appealProductLimitCounterPair.getLeft();
    ProductBusinessPartnerCounter businessPartnerCounter = appealProductLimitCounterPair.getRight();
    if (appealProductLimit <= businessPartnerCounter.getAppealedProductCount()) {
      log.info("Limit {} crossed for bp-code {}", businessPartnerCounter.getAppealedProductCount(),
        appealProductRequest.getBusinessPartnerCode());
      return AppealProductResponse.builder().errorCode(ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode())
        .errorMessage(
          String.format(ApiErrorCode.APPEAL_LIMIT_CROSSED.getDesc(), appealProductLimit)).build();
    } else {
      ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(
          appealProductRequest.getProductSku());
      if (Objects.isNull(productBusinessPartner) || !Constants.IN_PROGRESS_STATE.equals(
        productBusinessPartner.getState())) {
        return AppealProductResponse.builder()
          .errorCode(ApiErrorCode.PRODUCT_IN_INVALID_STATE.getCode())
          .errorMessage(ApiErrorCode.PRODUCT_IN_INVALID_STATE.getDesc()).build();
      }
      log.info("Updating appeal product in pbp for {}", appealProductRequest.getProductSku());
      updateProductCollectionAndCount(productBusinessPartner, businessPartnerCounter);
      return updateAppealProductInPdt(appealProductRequest);
    }
  }

  public void updateProductCollectionAndCount(ProductBusinessPartner productBusinessPartner,
    ProductBusinessPartnerCounter productBusinessPartnerCounter) {
    productBusinessPartnerCounter.setAppealedProductCount(
      productBusinessPartnerCounter.getAppealedProductCount() + 1);
    productBusinessPartner.setAppealedProduct(true);
    productBusinessPartnerCounterRepository.save(productBusinessPartnerCounter);
    productBusinessPartnerRepository.save(productBusinessPartner);
  }

  private AppealProductResponse updateAppealProductInPdt(AppealProductRequest appealProductRequest)
    throws ApplicationException {
    log.info("Updating appeal product updated in PDT for productCode = {} ",
      appealProductRequest.getProductCode());
    return productDistributionTaskRepository.updateAppealProduct(appealProductRequest);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void incrementCounterForProductAppeal(
      ProductBusinessPartnerCounter businessPartnerCounter) {
    businessPartnerCounter.setAppealedProductCount(
      businessPartnerCounter.getAppealedProductCount() + Constants.ONE);
    productBusinessPartnerCounterRepository.save(businessPartnerCounter);
  }

  @Override
  public Pair<Integer, ProductBusinessPartnerCounter> fetchThresholdAndCounterForAppealProduct(
      String storeId, String businessPartnerCode) {
    int appealProductLimit = Integer.parseInt(
        productSystemParameterService.findByStoreIdAndVariable(storeId,
            SystemParameterConstants.APPEAL_PRODUCT_LIMIT).getValue());
    ProductBusinessPartnerCounter businessPartnerCounter =
        productBusinessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(storeId,
            businessPartnerCode);
    if (Objects.isNull(businessPartnerCounter)) {
      businessPartnerCounter =
          ConverterUtil.generateBusinessPartnerCounter(storeId, businessPartnerCode);
    }
    return Pair.of(appealProductLimit, businessPartnerCounter);
  }

  @Override
  public void decrementCounterForProductAppeal(String storeId, String businessPartnerCode) {
    ProductBusinessPartnerCounter productBusinessPartnerCounter =
      productBusinessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(storeId,
        businessPartnerCode);
    productBusinessPartnerCounter.setAppealedProductCount(
      productBusinessPartnerCounter.getAppealedProductCount() - Constants.ONE);
    productBusinessPartnerCounterRepository.save(productBusinessPartnerCounter);
  }

  @Override
  public AppealProductConfigResponse fetchAppealProductConfig(String storeId, String businessPartnerCode) {
    AppealProductConfigResponse appealProductConfigResponse = new AppealProductConfigResponse();
    ProductBusinessPartnerCounter businessPartnerCounter =
      productBusinessPartnerCounterRepository.findByStoreIdAndBusinessPartnerCode(storeId,
        businessPartnerCode);
    int appealProductLimit = Integer.parseInt(
      productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.APPEAL_PRODUCT_LIMIT).getValue());
    if (Objects.isNull(businessPartnerCounter)) {
      appealProductConfigResponse.setEligible(true);
      appealProductConfigResponse.setPresentCount(0);

    } else {
      appealProductConfigResponse.setPresentCount(businessPartnerCounter.getAppealedProductCount());
      appealProductConfigResponse.setEligible(
        businessPartnerCounter.getAppealedProductCount() < appealProductLimit);
    }
    appealProductConfigResponse.setThresholdCount(appealProductLimit);
    return appealProductConfigResponse;
  }
}
