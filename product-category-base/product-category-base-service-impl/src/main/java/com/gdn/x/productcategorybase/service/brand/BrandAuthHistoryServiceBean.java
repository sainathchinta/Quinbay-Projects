package com.gdn.x.productcategorybase.service.brand;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthHistoryRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = true)
public class BrandAuthHistoryServiceBean implements BrandAuthHistoryService {

  @Autowired
  private BrandAuthHistoryRepository brandAuthHistoryRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;


  @Override
  public Page<BrandAuthHistoryResponse> findByBrandCodeAndSellerCode(String storeId,
    BrandAuthHistoryRequest brandAuthHistoryRequest, Pageable pageable) {
      Page<BrandAuthorisationHistory> brandAuthorisationHistories = brandAuthHistoryRepository
        .findByStoreIdAndBrandCodeAndSellerCodeOrderByCreatedDateDesc(storeId,
          brandAuthHistoryRequest.getBrandCode(), brandAuthHistoryRequest.getSellerCode(),
          pageable);
      List<BrandAuthHistoryResponse> brandAuthHistoryResponse =
        ConverterUtil.generateBrandAuthHistoryResponses(brandAuthorisationHistories , brandAuthHistoryRequest);
    return new PageImpl<>(brandAuthHistoryResponse, pageable, brandAuthorisationHistories.getTotalElements());
  }

  @Override
  public Page<BrandAuthHistoryResponse> getBrandAuthHistory(String storeId,
    BrandAuthHistoryRequest brandAuthHistoryRequest, int page, int size) {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandAuthHistoryRequest.getSellerCode()),
      String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandAuthHistoryRequest.getBrandCode()),
      String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    Pageable pageable = PageRequest.of(page, size);
    return findByBrandCodeAndSellerCode(storeId, brandAuthHistoryRequest, pageable);
  }

  @Override
  @Transactional(readOnly = false)
  public BrandAuthorisationHistory saveBrandAuthHistory(
    BrandAuthorisationHistory brandAuthorisationHistory) {
    return brandAuthHistoryRepository.save(brandAuthorisationHistory);
  }

  @Override
  public BrandAuthDomainEventModel generateBrandAuthHistoryDomainEventModel(
    BrandAuthorisationHistory brandAuthorisationHistory) {
    return domainEventPublisherService
      .publishBrandAuthHistoryEvent(brandAuthorisationHistory.getBrandCode(),
        brandAuthorisationHistory.getSellerCode(), brandAuthorisationHistory);
  }
}
