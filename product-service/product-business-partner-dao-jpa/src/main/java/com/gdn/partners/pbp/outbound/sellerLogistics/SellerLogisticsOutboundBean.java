package com.gdn.partners.pbp.outbound.sellerLogistics;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.sellerLogistics.feign.SellerLogisticsFeign;
import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.SaveSkuLogisticProductResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class SellerLogisticsOutboundBean implements SellerLogisticsOutbound {

  private static final String LOGISTICS_CLIENT_RESPONSE_NULL =
      "Logistics service client response is null";

  @Autowired
  private SellerLogisticsFeign sellerLogisticsFeign;

  public List<GetSkuLogisticProductResponse> getSkuLogistics(String itemSku, String merchantCode,
      String merchantDeliveryType) throws Exception {
    Response<List<GetSkuLogisticProductResponse>> getSkuLogisticsProductResponseList =
        sellerLogisticsFeign.getSkuLogistics(itemSku, merchantCode, merchantDeliveryType,
            GdnMandatoryRequestParameterUtil.getRequestId(), Constants.DEFAULT_CHANNEL_ID,
            GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getUsername());
    validateResponse(getSkuLogisticsProductResponseList);
    return getSkuLogisticsProductResponseList.getData();
  }

  public SaveSkuLogisticProductResponse saveSkuLogistics(SaveSkuLogisticProductRequest requestBody,
      boolean isActive) throws Exception {
    Response<SaveSkuLogisticProductResponse> saveSkuLogisticProductResponse =
        sellerLogisticsFeign.saveSkuLogistics(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), isActive, requestBody);
    validateResponse(saveSkuLogisticProductResponse);
    return saveSkuLogisticProductResponse.getData();
  }

  private void validateResponse(Response<?> clientResponse) throws Exception {
    if (Objects.isNull(clientResponse)) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, LOGISTICS_CLIENT_RESPONSE_NULL);
    }
    if (!(clientResponse.getCode() == HttpStatus.OK.value())
        || Objects.isNull(clientResponse.getData())) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          getSellerLogisticErrorCode(clientResponse.getErrors()));
    }
  }

  private static String getSellerLogisticErrorCode(Map<String, List<String>> errorMap) {
    if (Objects.nonNull(errorMap) && !errorMap.isEmpty()) {
      List<String> errorList = errorMap.values().stream().findFirst().orElseGet(ArrayList::new);
      if (CollectionUtils.isNotEmpty(errorList) && StringUtils.isNotBlank(errorList.get(0))) {
        return errorList.get(0);
      }
    }
    return ErrorCategory.UNSPECIFIED.getMessage();
  }
}
