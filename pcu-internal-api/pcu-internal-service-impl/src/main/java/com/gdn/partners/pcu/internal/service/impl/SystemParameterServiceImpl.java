package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import com.gdn.partners.pcu.internal.service.SystemParameterService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Slf4j
public class SystemParameterServiceImpl implements SystemParameterService {

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PDTFeign pdtFeign;

  @Value("#{${translationEnMap}}")
  private Map<String, String> valuesEnglishMap;

  @Value("#{${translationInMap}}")
  private Map<String, String> valuesIndonesiaMap;

  @Override
  public void updateSystemParameter(String storeId, String channelId,
      String clientId, String requestId, String username,
      List<SystemParameterRequest> productSystemParameterRequest) {
    for (SystemParameterRequest systemParameterRequest : productSystemParameterRequest) {
      systemParameterRequest.setShowOnUI(true);
      GdnBaseRestResponse response =
          pbpFeign.updateSystemParameter(storeId, channelId, clientId, requestId, username,
              systemParameterRequest);
      ResponseHelper.validateResponse(response);
    }
  }

  public GdnRestListResponse<ProductSystemParameterResponse> fetchSystemParameterShowOnUI(
      String storeId, String channelId, String clientId, String requestId) {
    GdnRestListResponse<ProductSystemParameterResponse> productSystemParameterShowOnUIResponse =
        pbpFeign.fetchSystemParameterShowOnUI(storeId, channelId, clientId, requestId);
    ResponseHelper.validateResponse(productSystemParameterShowOnUIResponse);
    List<ProductSystemParameterResponse> productSystemParameterResponses =
      populateTranslations(productSystemParameterShowOnUIResponse.getContent());
    productSystemParameterShowOnUIResponse.setContent(productSystemParameterResponses);
    return productSystemParameterShowOnUIResponse;
  }

  @Override
  public SystemParameterResponse getSystemParameterSwitches(String storeId, String requestId) {
    GdnRestSingleResponse<SystemParameterResponse> internalSystemParameterResponse =
      pdtFeign.fetchInternalSystemParameter(storeId, requestId);
    ResponseHelper.validateResponse(internalSystemParameterResponse);
    return new SystemParameterResponse(
      internalSystemParameterResponse.getValue().getProductSystemParameterSwitchValues());
  }

  private List<ProductSystemParameterResponse> populateTranslations(
    List<ProductSystemParameterResponse> parameterResponseList) {
    List<ProductSystemParameterResponse> responseList = new ArrayList<>();

    // Pre-fetch translation values
    Map<String, String> englishTranslations = new HashMap<>(valuesEnglishMap);
    Map<String, String> indonesiaTranslations = new HashMap<>(valuesIndonesiaMap);

    parameterResponseList.forEach(response -> {
      String englishTranslation =
        englishTranslations.getOrDefault(response.getVariable(), StringUtils.EMPTY);
      String indonesiaTranslation =
        indonesiaTranslations.getOrDefault(response.getVariable(), StringUtils.EMPTY);
      response.setVariableNameEn(englishTranslation);
      response.setVariableNameIn(indonesiaTranslation);
      responseList.add(response);
    });

    return responseList;
  }

}
