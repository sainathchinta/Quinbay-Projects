package com.gdn.x.product.outbound.impl;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import com.gdn.x.product.outbound.api.WarehouseItemMasterOutbound;
import com.gdn.x.product.outbound.api.feign.WarehouseItemMasterFeign;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
public class WarehouseItemMasterOutboundImpl implements WarehouseItemMasterOutbound {

  @Autowired
  private WarehouseItemMasterFeign warehouseItemMasterFeign;

  @Override
  public CreateUpdateBOMRecipeResponse createAndUpdateProductBundle(
    CreateUpdateBillOfMaterialRecipeCommandRequest recipeRequest) throws Exception {
    Response<CreateUpdateBOMRecipeResponse> response =
        warehouseItemMasterFeign.createUpdateProductBundle(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            recipeRequest);

    // check if api is success
    if (!HttpStatus.OK.getReasonPhrase().equals(response.getStatus())) {
      log.error("Error while creating bom  request : {} , response : {} ", recipeRequest, response);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, String.valueOf(response.getErrors()));
    }

    // check for cyclic bundling
    if (Optional.ofNullable(response.getErrors()).map(errors -> errors.get("message").contains("CYCLIC BOM FOUND"))
        .orElse(false)) {
      log.error("Cyclic dependency found request : {} , response : {} ", recipeRequest, response);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, String.valueOf(response.getErrors()));
    }

    return response.getData();
  }
}
