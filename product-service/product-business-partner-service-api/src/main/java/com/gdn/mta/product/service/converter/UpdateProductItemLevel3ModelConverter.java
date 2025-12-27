package com.gdn.mta.product.service.converter;

import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;

public interface UpdateProductItemLevel3ModelConverter {
  UpdateProductItemLevel3Model convertFromProductLevel3Summary(ProductLevel3Summary request);
  
  UpdateProductItemLevel3Model convertFromProductLevel3(ProductLevel3 request, boolean mapWithAttributeCode);
  
  UpdateProductItemLevel3Model convertFromProductAndItemsResponse(ProductAndItemsResponse request);
  
  UpdateProductItemLevel3Model convertFromProductLevel3Inventory(ProductLevel3Inventory request);

  UpdateProductItemLevel3Model convertFromProductLevel3Detail(ProductLevel3DetailResponse request, boolean mapWithAttributeCode);

  UpdateProductItemLevel3Model convertFromItemPickupPointListingResponse(
      ItemPickupPointListingResponse itemPickupPointListingResponse, Map<String, Boolean> addingWholeSale1stTimeL5s);

  UpdateProductItemLevel3Model convertFromItemPickupPoint(ItemPickupPoint itemPickupPoint,
      Set<String> wholeSaleFlagUpdatedL5s, UpdateProductItemLevel3Model savedData,
      Map<String, Boolean> addingWholeSale1stTimeL5s);

}
