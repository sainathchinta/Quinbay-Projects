package com.gdn.partners.pcu.internal.service.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.MTAFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import org.mockito.MockitoAnnotations;

public class MTAServiceImplTest {

  private static final String TYPE = "type";
  private static final String USER_NAME = "userName";
  private static final String CONTENT_REVIEWERS = "CONTENT_REVIEWERS";
  private static final String IMAGE_REVIEWERS = "IMAGE_REVIEWERS";
  private static final String TYPE_CONTENT_REVIEW = "content";
  private static final String TYPE_IMAGE_REVIEW = "image";

  private Map<String, List<String>> reviewerList;

  @InjectMocks
  private MTAServiceImpl mtaService;

  @Mock
  private MTAFeign mtaFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    reviewerList = new HashMap<>();
    reviewerList.put(TYPE, Collections.singletonList(USER_NAME));
  }
  @Test
  void getImageOrContentReviewersContentTest() {
    Mockito.when(mtaFeign.getImageOrContentReviewers(CONTENT_REVIEWERS))
        .thenReturn(new GdnRestSimpleResponse(Constants.REQUEST_ID, reviewerList));
    Map<String, List<String>> imageOrContentReviewers = mtaService.getImageOrContentReviewers(TYPE_CONTENT_REVIEW);
    Mockito.verify(mtaFeign).getImageOrContentReviewers(CONTENT_REVIEWERS);
    Assertions.assertEquals(reviewerList, imageOrContentReviewers);
  }

  @Test
  public void getImageOrContentReviewersImageTest() {
    Mockito.when(mtaFeign.getImageOrContentReviewers(IMAGE_REVIEWERS))
        .thenReturn(new GdnRestSimpleResponse(Constants.REQUEST_ID, reviewerList));
    Map<String, List<String>> imageOrContentReviewers = mtaService.getImageOrContentReviewers(TYPE_IMAGE_REVIEW);
    Mockito.verify(mtaFeign).getImageOrContentReviewers(IMAGE_REVIEWERS);
    Assertions.assertEquals(reviewerList, imageOrContentReviewers);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(mtaFeign);
  }
}