package com.gdn.mta.product.converter;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;

public interface ProductLevel3RequestConverter {

  ProductLevel3SummaryFilter convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(
      ProductLevel3SummaryRequest request);

  ProductLevel3SummaryFilter convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(
      ProductLevel3SummaryMinifiedRequest request);

  ProductLevel3ItemSearch convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(
      ProductLevel3ItemSearchRequest request);

  ProductLevel3SummaryFilterDetails convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(
      ProductLevel3SummaryDetailsRequest request);
}
