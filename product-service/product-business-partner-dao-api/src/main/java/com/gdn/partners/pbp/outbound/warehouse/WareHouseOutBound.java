package com.gdn.partners.pbp.outbound.warehouse;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;

public interface WareHouseOutBound {

  /**
   * create or update product bundle
   *
   * @param recipeRequest
   * @return
   */
  CreateUpdateBOMRecipeResponse createAndUpdateProductBundle(
    CreateUpdateBillOfMaterialRecipeCommandRequest recipeRequest);

}
