package com.gdn.mta.bulk.service;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.feignConfig.ProductAssemblyFeign;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAssemblyOutboundServiceBean implements ProductAssemblyOutboundService {

  @Autowired
  private ProductAssemblyFeign productAssemblyFeign;

  @Override
  public List<MasterWarehouseResponse> getWarehouseCodeAndFulfillmentCenter(String storeId, String page, String limit) {
    GdnRestListResponse<MasterWarehouseResponse> response =
        productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, page, limit);
    if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
      log.error("Error on fetching warehouse details for page {} : limit : {} storeId : {} ", page, limit, storeId);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public GdnBaseRestResponse assemblyDisassemblyRequest(String storeId, String type, String requestId, String username,
      SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest) {
    GdnBaseRestResponse response =
        productAssemblyFeign.assemblyDisassemblyRequest(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId,
            username, type, simpleListAssemblyDisassemblyRequest);
    if (!response.isSuccess()) {
      log.error("Error on creating assembly/disassembly request for request : {} ",
          simpleListAssemblyDisassemblyRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnBaseRestResponse transferRequest(String storeId, String requestId, String username,
      TransferRequest transferRequest) {
    GdnBaseRestResponse response =
        productAssemblyFeign.transferRequest(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username,
            transferRequest);
    if (!response.isSuccess()) {
      log.error("Error on creating transfer request for request : {} ", transferRequest);
      String errorMessage = StringUtils.replace(response.getErrorMessage(),
          BulkProcessValidationErrorMessages.CANNOT_PROCESS_INVALID_DATA, StringUtils.EMPTY, 1);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, errorMessage);
    }
    return response;
  }
}
