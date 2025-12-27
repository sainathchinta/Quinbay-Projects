package com.gdn.mta.bulk.service.download;

import java.util.List;

import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.dto.product.TaggedProductFilterDTO;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.response.BulkDownloadTaggedProductsResponse;
import com.gdn.mta.bulk.service.PriceAnalyticsOutboundService;

@Service
public class BulkDownloadTaggedProductsServiceBean implements BulkProcessDataService {

  @Autowired
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    TaggedProductFilterDTO taggedProductFilterDTO = (TaggedProductFilterDTO) request;
    TaggedProductFilterRequest taggedProductFilterRequest = new TaggedProductFilterRequest();
    BeanUtils.copyProperties(taggedProductFilterDTO, taggedProductFilterRequest);
    List<TaggedProductFilterResponse> taggedProductFilterRequestList =
        priceAnalyticsOutboundService.getDownloadTaggedProducts(taggedProductFilterRequest);
    BulkDownloadTaggedProductsResponse bulkDownloadTaggedProductsResponse = new BulkDownloadTaggedProductsResponse();
    bulkDownloadTaggedProductsResponse.setTaggedProductFilterResponseList(taggedProductFilterRequestList);
    bulkDownloadTaggedProductsResponse.setEmailAddress(taggedProductFilterRequest.getEmailAddress());
    bulkDownloadTaggedProductsResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    return bulkDownloadTaggedProductsResponse;
  }
}
