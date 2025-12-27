package com.gdn.partners.pbp.outbound.warehouse;

import java.util.ArrayList;
import java.util.Optional;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.outbound.warehouse.feign.WareHouseFeign;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class WareHouseOutBoundBean implements WareHouseOutBound {

  @Autowired
  private WareHouseFeign wareHouseFeign;

  @Override
  public CreateUpdateBOMRecipeResponse createAndUpdateProductBundle(
    CreateUpdateBillOfMaterialRecipeCommandRequest recipeRequest) {
    Response<CreateUpdateBOMRecipeResponse> response =
        wareHouseFeign.createUpdateProductBundle(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            recipeRequest);
    // check if api is success
    if (!(HttpStatus.OK.getReasonPhrase()).equals(response.getStatus())) {
      log.error("Error while creating bom  request : {} , response : {} ", recipeRequest, response);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, String.valueOf(response.getErrors()));
    }

    // check for cyclic bundling
    if (Optional.ofNullable(response.getErrors())
        .map(errors -> CollectionUtils.isNotEmpty(errors.getOrDefault("message", new ArrayList<>()))).orElse(false)) {
      log.error("Cyclic dependency found request : {} , response : {} ", recipeRequest, response);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, String.valueOf(response.getErrors()));
    }
    return response.getData();
  }

}
