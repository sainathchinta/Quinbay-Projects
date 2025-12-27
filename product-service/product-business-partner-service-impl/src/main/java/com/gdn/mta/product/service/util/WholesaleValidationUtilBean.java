package com.gdn.mta.product.service.util;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class WholesaleValidationUtilBean implements WholesaleValidationUtil {

  @Value("${max.wholesale.price.requests}")
  private int maxWholesalePriceRequests;

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public ApiErrorCode validateWholesaleConfigOnFlow1(String categoryCode,
      List<ProductItemCreationRequest> productItemCreationRequests, boolean validateAtL4Level) {
    ApiErrorCode apiErrorCode = null;
    GdnRestSingleResponse<WholesaleMappingResponse> wholesaleConfigToCategory = pcbFeign
        .getWholesaleConfigToCategory(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), categoryCode);
    if (!wholesaleConfigToCategory.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, String
          .format("Failed to get wholesale configurations for category : %s, error : {}", categoryCode,
              wholesaleConfigToCategory.getErrorMessage()));
    }
    if (Objects.nonNull(wholesaleConfigToCategory.getValue()) && wholesaleConfigToCategory.getValue()
        .isWholesalePriceConfigEnabled()) {
      for (ProductItemCreationRequest productItemCreationRequest : productItemCreationRequests) {
        if (validateAtL4Level) {
          if (CollectionUtils.isNotEmpty(productItemCreationRequest.getProductItemWholesalePriceRequests())
              && Boolean.TRUE.equals(productItemCreationRequest.getWholesalePriceActivated())) {
            try {
              CommonUtils
                  .validateWholesalePriceConfig(productItemCreationRequest.getProductItemWholesalePriceRequests(),
                      productItemCreationRequest.getSalePrice(), wholesaleConfigToCategory.getValue());
            } catch (ApiIncorrectInputDataException e) {
              productItemCreationRequest.setWholesalePriceActivated(false);
              apiErrorCode = ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE;
            }
          }
        } else {
          for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
            if (CollectionUtils.isNotEmpty(pickupPointCreateRequest.getProductItemWholesalePriceRequests())
                && Boolean.TRUE.equals(pickupPointCreateRequest.getWholesalePriceActivated())) {
              try {
                CommonUtils
                    .validateWholesalePriceConfig(pickupPointCreateRequest.getProductItemWholesalePriceRequests(),
                        pickupPointCreateRequest.getSalePrice(), wholesaleConfigToCategory.getValue());
              } catch (ApiIncorrectInputDataException e) {
                pickupPointCreateRequest.setWholesalePriceActivated(false);
                apiErrorCode = ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE;
              }
            }
          }
        }
      }
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode validateWholesaleConfigOnFlow2(String categoryCode,
      List<ProductItemBusinessPartnerRequest> productItemCreationRequests) {
    ApiErrorCode apiErrorCode = null;
    GdnRestSingleResponse<WholesaleMappingResponse> wholesaleConfigToCategory = pcbFeign
        .getWholesaleConfigToCategory(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), categoryCode);
    if (!wholesaleConfigToCategory.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, String
          .format("Failed to get wholesale configurations for category : %s, error : {}", categoryCode,
              wholesaleConfigToCategory.getErrorMessage()));
    }
    if (Objects.nonNull(wholesaleConfigToCategory.getValue()) && wholesaleConfigToCategory.getValue()
        .isWholesalePriceConfigEnabled()) {
      for (ProductItemBusinessPartnerRequest productItemCreationRequest : productItemCreationRequests) {
        if (CollectionUtils.isNotEmpty(productItemCreationRequest.getProductItemWholesalePriceRequests())
            && Boolean.TRUE.equals(productItemCreationRequest.getWholesalePriceActivated())) {
          try {
            CommonUtils.validateWholesalePriceConfig(productItemCreationRequest.getProductItemWholesalePriceRequests(),
                productItemCreationRequest.getSalePrice(), wholesaleConfigToCategory.getValue());
          } catch (ApiIncorrectInputDataException e) {
            productItemCreationRequest.setWholesalePriceActivated(false);
            apiErrorCode = ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE;
          }
        }
      }
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode validateWholesaleConfigOnUpdate(String categoryCode,
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests, ItemRequest itemRequest,
      Integer minimumPrice, String itemSku, Boolean wholesalePriceActivated,
      WholesaleMappingResponse wholesaleMappingResponse) {
    ApiErrorCode apiErrorCode = null;
    if (Boolean.TRUE.equals(wholesalePriceActivated) && CollectionUtils
        .isNotEmpty(productItemWholesalePriceRequests)) {
      if(Objects.isNull(wholesaleMappingResponse)) {
        GdnRestSingleResponse<WholesaleMappingResponse> wholesaleConfigToCategory =
            pcbFeign.getWholesaleConfigToCategory(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER), MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
                MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER), MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
                MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), categoryCode);
        if (!wholesaleConfigToCategory.isSuccess()) {
          throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
              String.format("Failed to get wholesale configurations for category : %s, error : {}", categoryCode,
                  wholesaleConfigToCategory.getErrorMessage()));
        }
        wholesaleMappingResponse = wholesaleConfigToCategory.getValue();
      }
      if (Objects.nonNull(wholesaleMappingResponse) && wholesaleMappingResponse
          .isWholesalePriceConfigEnabled()) {
        try {
          CommonUtils.validateWholesalePrice(productItemWholesalePriceRequests,
              itemRequest.getPrice().stream().findFirst().get().getOfferPrice(), minimumPrice,
              maxWholesalePriceRequests, itemSku);
          CommonUtils.validateWholesalePriceConfig(productItemWholesalePriceRequests,
              itemRequest.getPrice().stream().findFirst().get().getOfferPrice(), wholesaleMappingResponse);
        } catch (ApiIncorrectInputDataException e) {
          itemRequest.setWholesalePriceActivated(false);
          apiErrorCode = e.getErrorCode();
        }
      }
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode validateWholesalePriceRequestForUpdate(String storeId,
      ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest, String itemSku,
      ProductAndItemsResponse savedProductData, int minimumPrice) {
    ApiErrorCode apiErrorCode = null;
    if (Boolean.TRUE.equals(productPriceAndWholesaleRequest.getWholesalePriceActivated())) {
      String categoryCode = savedProductData.getProduct().getMasterCatalog().getCategory().getCategoryCode();
      GdnRestSingleResponse<WholesaleMappingResponse> wholesaleMappingResponse = this.pcbFeign
          .getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode);
      if (!wholesaleMappingResponse.isSuccess()) {
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, String
            .format("Failed to get wholesale configurations for category : %s, error : {} ", categoryCode,
                wholesaleMappingResponse.getErrorMessage()));
      }
      if(Objects.nonNull(wholesaleMappingResponse.getValue())) {
        if (wholesaleMappingResponse.getValue().isWholesalePriceConfigEnabled()) {
          try {
            CommonUtils.validateWholesalePrice(productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests(),
                productPriceAndWholesaleRequest.getOfferPrice(), minimumPrice, maxWholesalePriceRequests, itemSku);
            CommonUtils
                .validateWholesalePriceConfig(productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests(),
                    productPriceAndWholesaleRequest.getOfferPrice(), wholesaleMappingResponse.getValue());
          } catch (ApiIncorrectInputDataException e) {
            log.error("Wholesale rules didn't match category config, disabling wholesaleActivated flag");
            productPriceAndWholesaleRequest.setWholesalePriceActivated(Boolean.FALSE);
            apiErrorCode = e.getErrorCode();
          }
        }
      }
    }
    return apiErrorCode;
  }
}
