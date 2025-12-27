package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;

public class ProductLevel1RepositoryBeanTest {
  
  @InjectMocks
  private ProductLevel1RepositoryBean productLv1Repo;

  @Captor
  ArgumentCaptor<SummaryFilterRequest> argumentCaptor;

  @Captor
  ArgumentCaptor<GdnRestListRequest> argumentCaptor1;

  private GdnRestListResponse<ReviewProductResponse> reviewProductResponses;

  private Pageable pageable;
  private static final String DEFAULT_USERNAME = "USERNAME";
  private static final String DEFAULT_REQUEST_ID = "REQUEST_ID";
  private static final SummaryFilterRequest summaryFilterRequest =
      new SummaryFilterRequest();
  
  @BeforeEach
  public void init(){
    MockitoAnnotations.initMocks(this);
    ReviewProductResponse reviewProductResponse = new ReviewProductResponse();
    List<ReviewProductResponse> reviewProductResponseList = new ArrayList<>();
    reviewProductResponseList.add(reviewProductResponse);
    reviewProductResponses =
        new GdnRestListResponse<ReviewProductResponse>(null, null, true, reviewProductResponseList,
            null, REQUEST_ID);
    pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
  }
  
  private static String REQUEST_ID = "1234";
  private static String USERNAME = "username@mail.com";
  private static String BP_CODE = "TOQ-15130";
  private static int PAGE_NUMBER = 2;
  private static int PAGE_SIZE = 5;

  @Test
  public void replaceProductImagesWhenError() throws Exception {
    try{
      Assertions.assertThrows(ApplicationException.class,
          () -> productLv1Repo.replaceProductImages(REQUEST_ID, USERNAME, BP_CODE,
              new ReplaceProductImagesRequest()));
    } catch(Exception e){
      throw e;
    }
  }

  @Test
  public void getReviewProducts_whenSuccessFalseTest() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    GdnRestListRequest pageRequest = new GdnRestListRequest(PAGE_NUMBER, PAGE_SIZE);
    reviewProductResponses.setSuccess(false);
      Assertions.assertThrows(ApplicationException.class,
          () -> productLv1Repo.getReviewProducts(REQUEST_ID, USERNAME, summaryFilterRequest,
              pageable));
  }

  @Test
  public void getReviewProducts_whenConnectionFailureTest() throws Exception {
    SummaryFilterRequest summaryFilterRequest = new SummaryFilterRequest();
    GdnRestListRequest pageRequest = new GdnRestListRequest(PAGE_NUMBER, PAGE_SIZE);
    reviewProductResponses.setSuccess(false);
      Assertions.assertThrows(ApplicationException.class,
          () -> productLv1Repo.getReviewProducts(REQUEST_ID, USERNAME, summaryFilterRequest,
              pageable));
  }

  @Test
  public void getReviewProducts_Exception() throws Exception {
    GdnRestListRequest gdnRestListRequest = new GdnRestListRequest();
    GdnRestListResponse<ReviewProductResponse> gdnRestListResponse = new GdnRestListResponse<>();
    gdnRestListResponse.setSuccess(false);
    gdnRestListRequest.setPage(2);
    gdnRestListRequest.setSize(5);
   try {
      this.productLv1Repo.getReviewProducts(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, summaryFilterRequest, pageable);
    } catch (ApplicationException e) {

    }
  }
}
