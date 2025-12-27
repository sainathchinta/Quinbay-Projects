package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.repository.RestrictedKeywordRepository;
import com.google.common.collect.ImmutableSet;

public class RestrictedKeywordServiceTest {

  @Mock
  private RestrictedKeywordRepository restrictedKeywordRepository;

  @InjectMocks
  private RestrictedKeywordServiceImpl restrictedKeywordsService;


  private static final String STORE_ID = "10001";
  private static final String KEYWORD = "KEYWORD";
  private static final String KEYWORD2 = "KEYWORD2";
  private static final VerificationMode AT_LEAST_ONCE = times(1);
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  private RestrictedKeyword restrictedKeyword;
  private List<RestrictedKeyword> restrictedKeywordList;
  private List<String> keywordList;

  private static final String DEFAULT_STORE_ID = "storeId";
  private static final String KEYWORD_ID = "keywordId";
  private static PageRequest pageRequest = PageRequest.of(0, 10);

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeywordList = new ArrayList<>();
    restrictedKeywordList.add(restrictedKeyword);
    keywordList = new ArrayList<>();
    keywordList.add("alco");
  }

  @AfterEach
  public void postTest() {
    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    verifyNoMoreInteractions(this.restrictedKeywordRepository);
  }


  @Test
  public void findRestrictedKeywordByKeywordTest() {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(restrictedKeywordList, DEFAULT_PAGE_REQUEST, restrictedKeywordList.size());
    Mockito.when(restrictedKeywordRepository
        .findByStoreIdAndKeywordContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD, DEFAULT_PAGE_REQUEST))
        .thenReturn(restrictedKeywordPage);
    assertNotNull(
        this.restrictedKeywordsService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, DEFAULT_PAGE_REQUEST));
    verify(this.restrictedKeywordRepository, RestrictedKeywordServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndKeywordContainingIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD, DEFAULT_PAGE_REQUEST);
    assertNotNull(KEYWORD);
  }

  @Test
  public void saveRestrictedKeywordsTest() {
    this.restrictedKeywordsService.saveRestrictedKeywords(Collections.singletonList(restrictedKeyword));
    Mockito.verify(restrictedKeywordRepository).saveAll(Collections.singletonList(restrictedKeyword));
  }

  @Test
  public void findByKeywordsTest() {
    Mockito.when(this.restrictedKeywordRepository
        .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, Collections.singletonList(KEYWORD_ID)))
        .thenReturn(new ArrayList<>());
    this.restrictedKeywordsService.findByKeywordId(DEFAULT_STORE_ID, Collections.singletonList(KEYWORD_ID));
    Mockito.verify(this.restrictedKeywordRepository)
        .findByStoreIdAndIdInAndMarkForDeleteFalse(DEFAULT_STORE_ID, Collections.singletonList(KEYWORD_ID));
  }

  @Test
  public void findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalseTest() {
    Mockito.when(this.restrictedKeywordRepository
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, KEYWORD))
        .thenReturn(restrictedKeyword);
    this.restrictedKeywordsService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordRepository)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, KEYWORD);
  }

  @Test
  public void getDeletedRestrictedKeywordIdTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO1 = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD).build();
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO2 = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD2).build();
    CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO =
        CategoryKeywordsUpdateListDTO.builder().deletedRestrictedKeywords(new ArrayList<>(Arrays.asList(categoryKeywordsUpdateDTO1, categoryKeywordsUpdateDTO2)))
            .build();
    RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);

    Mockito.when(restrictedKeywordRepository.findByStoreIdAndKeywordInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        ImmutableSet.of(KEYWORD, KEYWORD2))).thenReturn(Arrays.asList(restrictedKeyword));

    restrictedKeywordsService.getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);

    Mockito.verify(restrictedKeywordRepository).findByStoreIdAndKeywordInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        ImmutableSet.of(KEYWORD, KEYWORD2));

    Assertions.assertEquals(1, categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD_ID, categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords().get(0).getKeywordId());
  }

  @Test
  public void getDeletedRestrictedKeywordIdNoKeyordFoundTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO1 = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD).build();
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO2 = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD2).build();
    CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO =
        CategoryKeywordsUpdateListDTO.builder().deletedRestrictedKeywords(new ArrayList<>(Arrays.asList(categoryKeywordsUpdateDTO1, categoryKeywordsUpdateDTO2)))
            .build();
    RestrictedKeyword restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);

    Mockito.when(restrictedKeywordRepository.findByStoreIdAndKeywordInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        ImmutableSet.of(KEYWORD, KEYWORD2))).thenReturn(null);

    restrictedKeywordsService.getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);

    Mockito.verify(restrictedKeywordRepository).findByStoreIdAndKeywordInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        ImmutableSet.of(KEYWORD, KEYWORD2));

    Assertions.assertEquals(0, categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords().size());
  }

  @Test
  public void getDeletedRestrictedKeywordIdAllHaveKeywordIdTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO1 = CategoryKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).build();
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO2 = CategoryKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD2).build();
    CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO =
        CategoryKeywordsUpdateListDTO.builder().deletedRestrictedKeywords(new ArrayList<>(Arrays.asList(categoryKeywordsUpdateDTO1, categoryKeywordsUpdateDTO2)))
            .build();
    restrictedKeywordsService.getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
    Assertions.assertEquals(2, categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords().size());
  }

  @Test
  public void getRestrictedKeywordForListingWithKeywordTest() throws Exception {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(restrictedKeywordList, DEFAULT_PAGE_REQUEST, restrictedKeywordList.size());
    Mockito.when(
        this.restrictedKeywordRepository.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            STORE_ID, KEYWORD, pageRequest)).thenReturn(restrictedKeywordPage);
    restrictedKeywordsService.getRestrictedKeywordForListing(STORE_ID, KEYWORD, pageRequest);
    Mockito.verify(this.restrictedKeywordRepository)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            STORE_ID, KEYWORD, pageRequest);
  }

  @Test
  public void getRestrictedKeywordForListingWithEmptyKeywordTest() throws Exception {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(restrictedKeywordList, DEFAULT_PAGE_REQUEST, restrictedKeywordList.size());
    Mockito.when(
        this.restrictedKeywordRepository.findByStoreIdAndMarkForDeleteFalseAndValidateOnUiNotNullAndValidateByDsNotNullOrderByUpdatedDateDesc(
            STORE_ID, pageRequest)).thenReturn(restrictedKeywordPage);
    restrictedKeywordsService.getRestrictedKeywordForListing(STORE_ID, StringUtils.EMPTY, pageRequest);
    Mockito.verify(this.restrictedKeywordRepository)
        .findByStoreIdAndMarkForDeleteFalseAndValidateOnUiNotNullAndValidateByDsNotNullOrderByUpdatedDateDesc(STORE_ID,
            pageRequest);
  }

  @Test
  public void getRestrictedKeywordForListingStoreIdEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordsService.getRestrictedKeywordForListing(StringUtils.EMPTY, KEYWORD, pageRequest));
  }

  @Test
  public void getListOfRestrictedKeywordsForUiValidationTest() {
    Mockito.when(this.restrictedKeywordRepository.findByStoreIdAndMarkForDeleteFalseAndValidateOnUi(STORE_ID,true))
        .thenReturn(restrictedKeywordList);
    restrictedKeywordsService.getListOfRestrictedKeywordsForUiValidation(STORE_ID);
    Mockito.verify(this.restrictedKeywordRepository).findByStoreIdAndMarkForDeleteFalseAndValidateOnUi(STORE_ID, true);
  }
}