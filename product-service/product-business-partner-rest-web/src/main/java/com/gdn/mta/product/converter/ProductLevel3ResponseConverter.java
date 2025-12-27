package com.gdn.mta.product.converter;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;

public interface ProductLevel3ResponseConverter {
  ProductLevel3SummaryResponse convertProductLevel3SummaryToProductLevel3SummaryResponse(
      ProductLevel3Summary source);

  ProductLevel3SummaryMinifiedResponse convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(
      ProductLevel3SummaryMinified source);

  ProductLevel3ItemSearchResponse convertProductLevel3ItemToProductLevel3ItemSearchResponse(
      ProductLevel3Item source);

  ProductLevel3SummaryDetailsResponse convertProductLevel3SummaryDetailsToProductLevel3SummaryDetailsResponse(
      ProductLevel3SummaryDetails productLevel3SummaryDetails);
}
