package com.gdn.x.product.outbound.api;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;

public interface WarehouseItemMasterOutbound {

  /**
   * create or update product bundle
   *
   * @param recipeRequest
   * @return
   */
  CreateUpdateBOMRecipeResponse createAndUpdateProductBundle(
    CreateUpdateBillOfMaterialRecipeCommandRequest recipeRequest)
      throws Exception;
}
