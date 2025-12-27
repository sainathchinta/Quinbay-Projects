package com.gdn.partners.pcu.internal.service.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductMTAWrapperImplTest {

  private static final String TYPE = "type";
  private static final String USER_NAME = "userName";

  private Map<String, List<String>> reviewerList;

  @InjectMocks
  private ProductMTAWrapperImpl productMTAWrapper;

  @Mock
  private MTAServiceImpl mtaService;

  @BeforeEach
  public void setUp() throws Exception {
    reviewerList = new HashMap<>();
    reviewerList.put(TYPE, Collections.singletonList(USER_NAME));
  }

  @Test
  public void getImageOrContentReviewersTest() throws Exception {
    Mockito.when(mtaService.getImageOrContentReviewers(TYPE)).thenReturn(reviewerList);
    Map<String, List<String>> imageOrContentReviewers = productMTAWrapper.getImageOrContentReviewers(TYPE);
    Mockito.verify(mtaService).getImageOrContentReviewers(TYPE);
    Assertions.assertEquals(reviewerList, imageOrContentReviewers);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(mtaService);
  }
}