package com.gdn.mta.bulk.util;

import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.mta.bulk.models.download.ReviewProductDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;

/**
 * Created by govind on 04/02/2019 AD.
 */
public class RequestConverterUtil {

  private static final String EMPTY_DROPDOWN_LIST_ELEMENT = "NA";

  private RequestConverterUtil() {
  }

  public static SummaryFilterRequest toSummaryFilterRequest(
      ReviewProductDownloadRequest reviewProductDownloadRequest) {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    BeanUtils.copyProperties(reviewProductDownloadRequest, summaryFilterRequest);
    return summaryFilterRequest;
  }

  public static FilterSummaryRequest fromVendorSummaryDownloadRequestToFilterSummaryRequest(
      VendorSummaryDownloadRequest vendorSummaryDownloadRequest) {
    FilterSummaryRequest filterSummaryRequest = new FilterSummaryRequest();
    BeanUtils.copyProperties(vendorSummaryDownloadRequest, filterSummaryRequest);
    filterSummaryRequest
        .setTimeFilterType(TimeFilterType.getTimeFilterTypeByValue(vendorSummaryDownloadRequest.getTimeFilterType()));
    if (EMPTY_DROPDOWN_LIST_ELEMENT.equals(vendorSummaryDownloadRequest.getAssigneeEmailId())) {
      filterSummaryRequest.setAssigneeEmailId(StringUtils.EMPTY);
    }
    return filterSummaryRequest;
  }
}