package com.gdn.mta.bulk.service.download;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.config.IprSourceDisplayNameProperties;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.IPRProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkIprProductDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
@Service
public class BulkDownloadIprProductsServiceBean implements BulkProcessDataService {

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private IprSourceDisplayNameProperties iprSourceDisplayNameProperties;

  @Value("${ipr.products.fetch.batch.size}")
  public int iprFetchBatchSize;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info("Fetching the data for bulk download of ipr product with request: {}", request);
    IPRProductsDownloadRequest iprProductsDownloadRequest = (IPRProductsDownloadRequest) request;
    BulkIprProductDownloadResponse bulkIprProductDownloadResponse =
        new BulkIprProductDownloadResponse();
    bulkIprProductDownloadResponse.setIprProductsResponses(
        fetchIprProductsResponse(request.getRequestId(), iprProductsDownloadRequest));
    bulkIprProductDownloadResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    return bulkIprProductDownloadResponse;
  }

  public List<IprProductsResponse> fetchIprProductsResponse(String requestId,
      IPRProductsDownloadRequest iprProductsDownloadRequest) {
    List<IprProductsResponse> iprProductsEntireResponses = new ArrayList<>();
    GdnRestListResponse<IprProductsResponse> response;
    int pageNumber = 0;
    int totalPages;
    do {
      response = productDistributionTaskRepository.getIprProductsList(requestId,
          constructIprProductListRequest(iprProductsDownloadRequest), pageNumber,
          iprFetchBatchSize);
      pageNumber++;
      totalPages = (int) Math.ceil(
          (double) response.getPageMetaData().getTotalRecords() / iprFetchBatchSize);
      iprProductsEntireResponses.addAll(response.getContent());
    } while (pageNumber < totalPages);
    iprProductsEntireResponses.forEach(iprProductsResponse -> {
      String rawSource = iprProductsResponse.getSource();
      if (StringUtils.isNotEmpty(rawSource)) {
        String transformedSource = Arrays.stream(rawSource.split(Constant.COMMA)).map(String::trim)
          .filter(StringUtils::isNotEmpty).map(iprSourceDisplayNameProperties.map::get)
          .filter(Objects::nonNull).collect(Collectors.joining(Constant.COMMA));
        iprProductsResponse.setSource(transformedSource);
      }
    });
    return iprProductsEntireResponses;
  }

  private IPRProductListRequest constructIprProductListRequest(
      IPRProductsDownloadRequest iprProductsDownloadRequest) {
    return IPRProductListRequest.builder().keyword(iprProductsDownloadRequest.getKeyword())
        .assignedTo(iprProductsDownloadRequest.getAssignedTo())
        .brandCode(iprProductsDownloadRequest.getBrandCode())
        .categoryCode(iprProductsDownloadRequest.getCategoryCode())
        .state(iprProductsDownloadRequest.getState())
        .timeFilterWebType(iprProductsDownloadRequest.getTimeFilterWebType())
        .sortOrder(iprProductsDownloadRequest.getSortOrder())
        .assigned(iprProductsDownloadRequest.getAssigned())
        .businessPartnerCode(iprProductsDownloadRequest.getBusinessPartnerCode()).build();
  }
}
