package com.gdn.x.productcategorybase.helper;

import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;


public class RestrictedKeywordServiceHelperTest {

  private static final String STORE_ID = "10001";
  private RestrictedKeyword restrictedKeyword;

  private RestrictedKeyword restrictedKeyword2;
  private Pageable pageable = PageRequest.of(1, 10);

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  @InjectMocks
  private RestrictedKeywordServiceHelper restrictedKeywordServiceHelper;

  @BeforeEach
  public void init() {
    initMocks(this);
    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword("tes");
    restrictedKeyword.setMarkForDelete(false);
    restrictedKeyword.setValidateByDs(Boolean.TRUE);
    restrictedKeyword.setValidateOnUi(Boolean.FALSE);

    restrictedKeyword2 = new RestrictedKeyword();
    restrictedKeyword2.setKeyword("test");
    restrictedKeyword2.setMarkForDelete(false);
    restrictedKeyword2.setValidateByDs(Boolean.TRUE);
    restrictedKeyword2.setValidateOnUi(Boolean.FALSE);
  }

  @Test
  public void findCategoryRestrictedKeywordsTest() throws Exception {
    String keyword = "test";
    Page<RestrictedKeyword> restrictedKeywordPage =
      new PageImpl<>(Arrays.asList(restrictedKeyword), pageable,
        Arrays.asList(restrictedKeyword).size());
    Mockito.when(
      this.restrictedKeywordService.getRestrictedKeywordSuggestions(anyString(), anyString(),
        any())).thenReturn(restrictedKeywordPage);
    Page<RestrictedKeywordsResponse> keywordsResponses =
      this.restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, keyword,
        pageable);
    verify(restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, keyword, pageable);
    assertNotNull(restrictedKeyword);
    assertEquals(Arrays.asList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(restrictedKeyword.getValidateByDs(),
      keywordsResponses.getContent().get(0).getValidateByDs());
  }

  @Test
  public void findCategoryRestrictedKeywordsWithEmptyKeywordTest() throws Exception {
    String keyword = "";
    Page<RestrictedKeyword> restrictedKeywordPage =
      new PageImpl<>(Arrays.asList(restrictedKeyword), pageable,
        Arrays.asList(restrictedKeyword).size());
    this.restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, keyword, pageable);
    verify(restrictedKeywordService, times(0)).getRestrictedKeywordSuggestions(STORE_ID, keyword,
      pageable);
    assertNotNull(restrictedKeyword);
    assertEquals(Arrays.asList(restrictedKeyword), restrictedKeywordPage.getContent());
  }

  @Test
  public void testStringLengthSort() {
    String keyword = "tes";
    Page<RestrictedKeyword> restrictedKeywordPage =
      new PageImpl<>(Arrays.asList(restrictedKeyword2, restrictedKeyword), pageable,
        Arrays.asList(restrictedKeyword).size());
    Mockito.when(
      this.restrictedKeywordService.getRestrictedKeywordSuggestions(anyString(), anyString(),
        any())).thenReturn(restrictedKeywordPage);
    Page<RestrictedKeywordsResponse> keywordsResponses =
      this.restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(STORE_ID, keyword,
        pageable);
    verify(restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, keyword, pageable);
    assertNotNull(restrictedKeyword);
    assertTrue(keywordsResponses.getContent().get(0).getKeyword().length() < keywordsResponses.getContent().get(1).getKeyword().length());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(restrictedKeywordService);
  }
}
