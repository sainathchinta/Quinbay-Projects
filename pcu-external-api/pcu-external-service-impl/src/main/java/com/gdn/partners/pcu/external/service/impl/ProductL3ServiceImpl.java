package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.ProductL3Service;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Service
@Slf4j
public class ProductL3ServiceImpl implements ProductL3Service {

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Value("${product.detail.page.url.prefix}")
  private String productDetailPageUrlPrefix;
  private static final String ACTIVE = "ACTIVE";

  @Value("${resize.image.path.removal}")
  private boolean resizeImageRemoval;

  @Value("${resize.image.path.list}")
  private String resizeImagePathList;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${override.product.editable.flag.based.on.synchronize}")
  private boolean overrideProductEditableFlagBasedOnSynchronize;

  @Override
  public ProductL3DetailWebResponse getL3DetailsByProductSku(String storeId, boolean isFbbFetchRequired,
      String businessPartnerCode, boolean isNeedCorrection, String productSku, boolean concatenateValueWithValueType) {
    GdnPreconditions.checkArgument(Constants.PRODUCT_SKU_PATTERN.matcher(productSku).matches(),
        ErrorMessages.INVALID_PATTERN_SKU);
    GdnPreconditions.checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    ProfileResponse businessPartner = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE.equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    GdnRestSingleResponse<ProductL3DetailsResponse> response =
      pbpFeign.getL3ProductDetailsByProductSku(productSku, isNeedCorrection, concatenateValueWithValueType);
    ResponseHelper.validateResponse(response);
    ProductL3DetailWebResponse productL3DetailWebResponse = ResponseHelper
        .toProductL3DetailWebResponse(response.getValue(),
            RequestHelper.toProductDetailPage(response.getValue().getProductSku(),
              productDetailPageUrlPrefix), isFbbFetchRequired);
    if (StringUtils.isNotBlank(response.getValue().getSizeChartCode())
        && sizeChartAdditionForProduct) {
      String sizeChartCode = response.getValue().getSizeChartCode();
      GdnRestSingleResponse<BasicSizeChartDetailMapResponse> sizeChartDetailResponse =
          pcbFeign.getBasicSizeChartDetails(Collections.singletonList(sizeChartCode));
      ResponseHelper.validateResponse(sizeChartDetailResponse);
      if (Objects.nonNull(sizeChartDetailResponse.getValue().getBasicSizeChartDetailResponseMap()
          .get(sizeChartCode))) {
        productL3DetailWebResponse.setSizeChartName(
            sizeChartDetailResponse.getValue().getBasicSizeChartDetailResponseMap()
                .get(sizeChartCode).getSizeChartName());
        productL3DetailWebResponse.setSizeChartBusinessPartnerCode(
            sizeChartDetailResponse.getValue().getBasicSizeChartDetailResponseMap()
                .get(sizeChartCode).getBusinessPartnerCode());
      }
    }
    if (!isNeedCorrection && resizeImageRemoval && CollectionUtils.isNotEmpty(productL3DetailWebResponse.getCommonImages())) {
      List<String> pathsToRemove = Arrays.asList(resizeImagePathList.split(Constants.COMMA_DELIMITER_NO_SPACE));
      productL3DetailWebResponse.getCommonImages()
          .removeIf(image -> containsAnyPath(image.getLocationPath(), pathsToRemove));
    }
    String merchantType = ResponseHelper
      .getMerchantTypeFromPurchaseTermAndInventoryFulfillment(businessPartner.getCompany().getPurchaseTerm(),
        businessPartner.getCompany().getInventoryFulfillment());
    if (Constants.MERCHANT_TYPE_TD.equalsIgnoreCase(merchantType)) {
      productL3DetailWebResponse.setCogsViewable(true);
    }
    if (StringUtils.isNotBlank(productL3DetailWebResponse.getVideoUrl())) {
      productL3DetailWebResponse.setUrl(StringUtils.EMPTY);
    }
    if (overrideProductEditableFlagBasedOnSynchronize) {
      if (Boolean.FALSE.equals(productL3DetailWebResponse.getSynchronize())) {
        productL3DetailWebResponse.setProductEditable(false);
      }
    }
    return productL3DetailWebResponse;
  }

  private boolean containsAnyPath(String locationPath, List<String> pathsToRemove) {
    locationPath = Optional.ofNullable(locationPath).orElse(StringUtils.EMPTY);
    return pathsToRemove.stream().anyMatch(locationPath::contains);
  }
}
