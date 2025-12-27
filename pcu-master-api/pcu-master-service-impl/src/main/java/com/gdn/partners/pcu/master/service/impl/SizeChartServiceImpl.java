package com.gdn.partners.pcu.master.service.impl;

import java.util.Arrays;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.client.model.BooleanResponse;

import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.SizeChartDeletionEventModel;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.master.service.SizeChartService;
import com.gdn.partners.pcu.master.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class SizeChartServiceImpl implements SizeChartService {

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${validate.product.accessibility}")
  private boolean validateProductAccessibility;

  @Value("${product.accessibility.list}")
  private String productAccessibilityList;

  @Override
  public GdnBaseRestResponse upsertSizeChart(String storeId, SizeChartRequest sizeChartRequest) {
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility,
        Arrays.asList(Credential.getAccessibilities()), Boolean.parseBoolean(clientParameterHelper.isExternal()),
        productAccessibilityList);
    GdnPreconditions.checkArgument(ResponseHelper.isSizeChartNameValid(sizeChartRequest.getName()),
        ErrorCodes.SIZE_CHART_NAME_EXCEEDED_LENGTH.getErrorMessage());
    GdnBaseRestResponse response = pcbFeign.upsertSizeChart(sizeChartRequest);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public SizeChartResponse fetchSizeChart(String sizeChartCode, boolean preview) {
    GdnRestSingleResponse<SizeChartResponse> sizeChartResponse =
      pcbFeign.fetchSizeChartDetails(sizeChartCode, preview);
    ResponseHelper.validateResponseByErrorCode(sizeChartResponse);
    return sizeChartResponse.getValue();
  }

  @Override
  public void deleteSizeChart(String sizeChartCode) {
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility,
        Arrays.asList(Credential.getAccessibilities()), Boolean.parseBoolean(clientParameterHelper.isExternal()),
        productAccessibilityList);
    String businessPartnerCode = clientParameterHelper.getBusinessPartnerCode();
    GdnRestListResponse<ProductSkuSizeChartResponse> response =
        xProductFeign.checkAnyProductsMappedToSizeChart(Constants.INT_ZERO, Constants.ONE,
            sizeChartCode);
    ResponseHelper.validateResponseByErrorCode(response);
    if (!response.getContent().isEmpty()) {
      throw new ValidationException(ErrorCodes.SIZE_CHART_CANNOT_BE_DELETED.getErrorCode(),
          ErrorCodes.SIZE_CHART_CANNOT_BE_DELETED.getErrorMessage());
    }
    GdnBaseRestResponse sizeChartResponse =
        pcbFeign.updateSizeChartStatus(sizeChartCode, true, true, businessPartnerCode);
    ResponseHelper.validateResponseByErrorCode(sizeChartResponse);
    kafkaPublisher.send(kafkaTopicProperties.getDeleteSizeChartEventName(), sizeChartCode,
        SizeChartDeletionEventModel.builder().sizeChartCode(sizeChartCode)
            .requestId(clientParameterHelper.getRequestId())
            .username(clientParameterHelper.getUsername())
            .businessPartnerCode(businessPartnerCode)
            .storeId(clientParameterHelper.getStoreId()).build());
  }

  @Override
  public GdnRestListResponse<SizeChartFilterResponse> filter(int page, int size,
      SizeChartFilterRequest request) {
    GdnRestListResponse<SizeChartFilterResponse> response = pcbFeign.filter(page, size, request);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public GdnRestSingleResponse<SizeChartResponse> validate(String sizeChartName,
      String businessPartnerCode) {
    GdnRestSingleResponse<SizeChartResponse> response =
        pcbFeign.findBySizeChartNameAndBusinessPartnerCode(sizeChartName, businessPartnerCode);
    ResponseHelper.validateResponseByErrorCodeExcludeValue(response);
    return response;
  }

  @Override
  public BooleanResponse validateCategory(String categoryCode, String sizeChartCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        pcbFeign.validateCategoryCode(categoryCode, sizeChartCode);
    ResponseHelper.validateResponse(response);
    return new BooleanResponse(response.getValue().getResult());
  }
}
