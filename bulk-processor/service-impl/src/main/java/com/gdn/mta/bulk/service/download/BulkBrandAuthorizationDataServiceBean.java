package com.gdn.mta.bulk.service.download;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.BrandAuthDownloadRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.bulk.util.ResponseHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;

import lombok.extern.slf4j.Slf4j;

@Service("bulkBrandAuthorizationDataServiceBean")
@Slf4j
public class BulkBrandAuthorizationDataServiceBean implements BulkProcessDataService {

  @Value("${simple.date.format}")
  private String simpleDateFormat;

  @Value("${brand.auth.download.size}")
  private int brandAuthDownloadSize;

  @Value("${fetch.seller.name.size}")
  private int fetchSellerNameSize;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    List<BrandAuthResponse> brandAuthResponseList = new ArrayList<>();
    BrandAuthDownloadRequest brandAuthDownloadRequest = (BrandAuthDownloadRequest) request;
    getBrandAuthorization(brandAuthDownloadRequest, brandAuthResponseList);
    return new BrandAuthDataResponse(brandAuthResponseList);
  }

  private void getBrandAuthorization(BrandAuthDownloadRequest brandAuthDownloadRequest,
      List<BrandAuthResponse> brandAuthResponseList) throws Exception {
    List<BrandAuthFilterResponse> brandAuthFilterResponses = new ArrayList<>();
    SimpleDateFormat dateFormat = new SimpleDateFormat(simpleDateFormat);
    int page = 0;
    long total = 0;
    do {
      GdnRestListResponse<BrandAuthFilterResponse> response =
          pcbOutboundService.getAuthorisations(Constant.STORE_ID, page, brandAuthDownloadSize,
              RequestHelper.toBrandAuthFilterRequest(brandAuthDownloadRequest));
      brandAuthFilterResponses.addAll(response.getContent());
      total = response.getPageMetaData().getTotalRecords();
      page++;
    } while (page * brandAuthDownloadSize < total);

    if (CollectionUtils.isNotEmpty(brandAuthFilterResponses)) {
      Map<String, String> sellerCodeSellerNameMap = new HashMap<>();
      List<String> businessPartnerCodes =
        brandAuthFilterResponses.stream().map(BrandAuthFilterResponse::getSellerCode).distinct()
          .collect(Collectors.toList());
      List<List<String>> bpCodesPartition =
        Lists.partition(businessPartnerCodes, fetchSellerNameSize);
      for (List<String> bpCodeList : bpCodesPartition) {
        BusinessPartnerFilterRequest businessPartnerFilterRequest =
          new BusinessPartnerFilterRequest();
        businessPartnerFilterRequest.setBusinessPartnerCodes(new HashSet<>(bpCodeList));
        List<ProfileResponse> profileResponsesList =
          businessPartnerRepository.filterByBusinessPartnerCodeList(businessPartnerFilterRequest, 0,
            bpCodeList.size());
        Map<String, String> sellerCodeNameMap =
          Optional.ofNullable(profileResponsesList).orElseGet(ArrayList::new).stream()
            .filter(Objects::nonNull).collect(
              Collectors.toMap(ProfileResponse::getBusinessPartnerCode,
                profileResponse -> profileResponse.getCompany().getBusinessPartnerName()));
        sellerCodeSellerNameMap.putAll(sellerCodeNameMap);
      }
      ResponseHelper.setBrandAuthResponse(brandAuthResponseList, brandAuthFilterResponses,
        sellerCodeSellerNameMap, dateFormat);
    }
  }

}
