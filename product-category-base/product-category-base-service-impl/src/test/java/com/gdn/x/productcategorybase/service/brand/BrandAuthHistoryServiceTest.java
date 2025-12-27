package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthHistoryRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.ArrayList;
import java.util.List;

public class BrandAuthHistoryServiceTest {

  @InjectMocks
  private BrandAuthHistoryServiceBean brandAuthHistoryService;

  @Mock
  private BrandAuthHistoryRepository brandAuthHistoryRepository;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  private static final String DEFAULT_BRAND_CODE = "BR-0001";
  private static final String DEFAULT_SELLER_CODE = "SR-0001";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static int PAGE = 0;
  private static int SIZE = 10;
  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private BrandAuthHistoryRequest brandAuthHistoryRequest;
  private BrandAuthHistoryResponse brandAuthHistoryResponse;
  private List<BrandAuthorisationHistory> brandAuthHistoryResponseList = new ArrayList<>();
  private BrandAuthorisationHistory brandAuthHistory = new BrandAuthorisationHistory();
  private Page<BrandAuthorisationHistory> page;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    brandAuthHistoryRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthHistoryRequest.setSellerCode(DEFAULT_SELLER_CODE);

    brandAuthHistory.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthHistory.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthHistory.setActivity(ACTIVITY_AUTH);
    brandAuthHistory.setOldStatus(OLD_VALUE_AUTH);
    brandAuthHistory.setNewStatus(NEW_VALUE_AUTH);
    brandAuthHistoryResponseList.add(brandAuthHistory);
    page = new PageImpl<>(brandAuthHistoryResponseList);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.brandAuthHistoryRepository);
  }

  @Test
  public void getBrandAuthHistoryTest(){
    Mockito.when(brandAuthHistoryRepository
      .findByStoreIdAndBrandCodeAndSellerCodeOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, pageable)).thenReturn(page);
    Page<BrandAuthHistoryResponse> response =
      brandAuthHistoryService.getBrandAuthHistory(DEFAULT_STORE_ID,brandAuthHistoryRequest,PAGE,SIZE);
    Mockito.verify(brandAuthHistoryRepository)
      .findByStoreIdAndBrandCodeAndSellerCodeOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, pageable);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, response.getContent().get(0).getBrandCode());
    Assertions.assertEquals(1, response.getTotalElements());
  }

  @Test
  public void generateBrandAuthHistoryTest(){
    BrandAuthDomainEventModel brandAuthDomainEventModel =
      BrandAuthDomainEventModel.builder().brandCode(DEFAULT_BRAND_CODE).build();
    Mockito.when(domainEventPublisherService.publishBrandAuthHistoryEvent(DEFAULT_BRAND_CODE,
      DEFAULT_SELLER_CODE,brandAuthHistory)).thenReturn(brandAuthDomainEventModel);
    BrandAuthDomainEventModel eventModel =
      brandAuthHistoryService.generateBrandAuthHistoryDomainEventModel(brandAuthHistory);
    Assertions.assertEquals(DEFAULT_BRAND_CODE,eventModel.getBrandCode());
  }

  @Test
  public void brandAuthSaveTest() {
    brandAuthHistoryService.saveBrandAuthHistory(brandAuthHistory);
    Mockito.verify(brandAuthHistoryRepository).save(brandAuthHistory);
  }
}
