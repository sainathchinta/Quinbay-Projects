package com.gdn.x.productcategorybase.service.brand;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.brand.BrandWipHistoryRepository;

public class BrandWipHistoryServiceTest {

  private BrandWipHistory brandWipHistory = new BrandWipHistory();
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private static final String DEFAULT_BRAND_CODE = "BR-0001";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BRD-0001-0001";
  private static final String DEFAULT_DESCRIPTION = "In review";
  private static final String DEFAULT_DESCRIPTION_REJECT = "Rejected";
  private static final String DEFAULT_NOTES = "NOTES";
  private static final String HYPHEN = "-";
  private static final String REJECTED_DESCRIPTION = DEFAULT_DESCRIPTION_REJECT + "-";
  private BrandWip brandWip = new BrandWip();
  private static int PAGE = 0;
  private static int SIZE = 10;
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private BrandWipHistory brandWipHistoryResponse = new BrandWipHistory();
  private List<BrandWipHistory> brandWipHistoryResponseList = new ArrayList<>();
  private Page<BrandWipHistory> page;
  private Pageable pageable = PageRequest.of(PAGE, SIZE);

  @Mock
  private BrandWipHistoryRepository brandWipHistoryRepository;

  @InjectMocks
  private BrandWipHistoryServiceBean brandWipHistoryService;

  @Captor
  private ArgumentCaptor<BrandWipHistory> brandWipHistoryArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    brandWipHistory.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipHistory.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistory.setDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWipHistory.setState(BrandWipState.DRAFT);
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    brandWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip.setBrandDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWip.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip.setState(BrandWipState.DRAFT);
    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipHistoryResponse.setState(BrandWipState.DRAFT);
    brandWipHistoryResponseList.add(brandWipHistoryResponse);
    page = new PageImpl<>(brandWipHistoryResponseList);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.brandWipHistoryRepository);
  }

  @Test
  public void generateBrandWipHistoryTest() {
    Mockito.when(this.brandWipHistoryRepository.save(brandWipHistory)).thenReturn(brandWipHistory);
    this.brandWipHistoryService.generateBrandWipHistory(brandWip, null, GdnMandatoryParameterUtil.getUsername());
    Mockito.verify(this.brandWipHistoryRepository).save(brandWipHistoryArgumentCaptor.capture());
    Assertions.assertEquals(brandWipHistory.getBrandCode(), brandWipHistoryArgumentCaptor.getValue().getBrandCode());
    Assertions.assertEquals(brandWipHistory.getState(), brandWipHistoryArgumentCaptor.getValue().getState());
  }

  @Test
  public void generateBrandWipHistoryTest_Rejected() {
    brandWip.setState(BrandWipState.REJECTED);
    brandWip.setNotes(DEFAULT_NOTES.getBytes());
    brandWipHistory.setState(BrandWipState.REJECTED);
    brandWipHistory.setDescription((REJECTED_DESCRIPTION + REJECTED_DESCRIPTION).getBytes());
    Mockito.when(this.brandWipHistoryRepository.save(brandWipHistory)).thenReturn(brandWipHistory);
    this.brandWipHistoryService
        .generateBrandWipHistory(brandWip, REJECTED_DESCRIPTION, GdnMandatoryParameterUtil.getUsername());
    Mockito.verify(this.brandWipHistoryRepository).save(brandWipHistoryArgumentCaptor.capture());
    Assertions.assertEquals(brandWipHistory.getBrandCode(), brandWipHistoryArgumentCaptor.getValue().getBrandCode());
    Assertions.assertEquals(brandWipHistory.getState(), brandWipHistoryArgumentCaptor.getValue().getState());
  }

  @Test
  public void findByBrandCodeTest() {
   Mockito.when(this.brandWipHistoryRepository.findByBrandCodeOrderByCreatedDateDesc(DEFAULT_BRAND_CODE, pageable)).thenReturn(page);
   Page<BrandWipHistoryResponse> responses = this.brandWipHistoryService.findByBrandCode(DEFAULT_BRAND_CODE, pageable);
   Mockito.verify(this.brandWipHistoryRepository).findByBrandCodeOrderByCreatedDateDesc(DEFAULT_BRAND_CODE, pageable);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, responses.getContent().get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, responses.getContent().get(0).getBrandRequestCode());
  }

  @Test
  public void findByBrandRequestCodeTest() {
    Mockito.when(this.brandWipHistoryRepository.findByBrandRequestCodeOrderByCreatedDateDesc(DEFAULT_BRAND_REQUEST_CODE, pageable)).thenReturn(page);
    Page<BrandWipHistoryResponse> responses =
        this.brandWipHistoryService.findByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE, pageable);
    Mockito.verify(this.brandWipHistoryRepository).findByBrandRequestCodeOrderByCreatedDateDesc(DEFAULT_BRAND_REQUEST_CODE, pageable);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, responses.getContent().get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, responses.getContent().get(0).getBrandRequestCode());
  }

  @Test
  void saveBrandWipHistoryTest() {
    BrandHistoryEventModel brandHistoryEventModel = new BrandHistoryEventModel();
    brandHistoryEventModel.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipHistoryService.saveBrandWipHistory(brandHistoryEventModel);
    Mockito.verify(brandWipHistoryRepository).save(brandWipHistoryArgumentCaptor.capture());
    Assertions.assertEquals(brandHistoryEventModel.getBrandCode(),
      brandWipHistoryArgumentCaptor.getValue().getBrandCode());
  }
}
