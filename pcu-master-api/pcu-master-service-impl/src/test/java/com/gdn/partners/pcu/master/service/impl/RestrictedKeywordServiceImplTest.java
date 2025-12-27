package com.gdn.partners.pcu.master.service.impl;

import java.util.Arrays;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.master.web.model.request.RestrictedKeywordsSearchWebRequest;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;

public class RestrictedKeywordServiceImplTest {

  @InjectMocks
  private RestrictedKeywordServiceImpl restrictedKeywordService;

  @Mock
  private PCBFeign pcbFeign;

  private RestrictedKeywordsSearchWebRequest keywordsSearchWebRequest;
  private RestrictedKeywordsSearchRequest keywordsSearchRequest;
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD = "keyword";
  private static final String NEW_VALUE = "newValue";
  private static final int page = 0;
  private static final int size = 10;
  private RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse;

  private RestrictedKeywordsResponse restrictedKeywordsResponse;
  private static final String REQUEST_ID = "requestId";
  private PageMetaData pageMetaData;
  private static final int TOTAL_RECORD = 1;
  private UiValidationRestrictedKeywordsResponse uiValidationRestrictedKeywordsResponse;
  private RestrictedKeywordsListingResponse restrictedKeywordsListingResponse;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    restrictedKeywordHistoryResponse = new RestrictedKeywordHistoryResponse();
    restrictedKeywordHistoryResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordHistoryResponse.setNewValue(NEW_VALUE);

    restrictedKeywordsResponse = new RestrictedKeywordsResponse();
    restrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsResponse.setValidateOnUi(Boolean.TRUE);
    restrictedKeywordsResponse.setValidateByDs(Boolean.FALSE);

    pageMetaData = new PageMetaData(page, size, TOTAL_RECORD);

    uiValidationRestrictedKeywordsResponse = new UiValidationRestrictedKeywordsResponse();
    uiValidationRestrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    uiValidationRestrictedKeywordsResponse.setKeyword(KEYWORD);

    restrictedKeywordsListingResponse = new RestrictedKeywordsListingResponse();
    restrictedKeywordsListingResponse.setKeyword(KEYWORD);
    restrictedKeywordsListingResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsListingResponse.setValidateByDs(true);
    restrictedKeywordsListingResponse.setValidateOnUi(true);

    keywordsSearchRequest = new RestrictedKeywordsSearchRequest();
    keywordsSearchRequest.setKeyword("keyword");

    keywordsSearchWebRequest = new RestrictedKeywordsSearchWebRequest();
    keywordsSearchWebRequest.setKeyword("keyword");
  }

  @AfterEach
  void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void getRestrictedKeywordHistoryTest() throws Exception {
    GdnRestListResponse<RestrictedKeywordHistoryResponse> responseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(restrictedKeywordHistoryResponse), pageMetaData,
            REQUEST_ID);
    Mockito.when(pcbFeign.getRestrictedKeywordHistory(KEYWORD_ID, page, size)).thenReturn(responseGdnRestListResponse);
    restrictedKeywordService.getRestrictedKeywordHistory(KEYWORD_ID, page, size);
    Mockito.verify(pcbFeign).getRestrictedKeywordHistory(KEYWORD_ID, page, size);
  }

  @Test
  void getRestrictedKeywordsTest() throws Exception {
    GdnRestListResponse<RestrictedKeywordsResponse> responseGdnRestListResponse =
      new GdnRestListResponse<>(null, null, true, Arrays.asList(restrictedKeywordsResponse), pageMetaData,
        REQUEST_ID);
    Mockito.when(pcbFeign.getRestrictedKeywords(keywordsSearchRequest, page, size)).thenReturn(responseGdnRestListResponse);
    Page<RestrictedKeywordsWebResponse> responses =
      restrictedKeywordService.getRestrictedKeywords(keywordsSearchWebRequest, page, size);
    Mockito.verify(pcbFeign).getRestrictedKeywords(keywordsSearchRequest, page, size);
    Assertions.assertEquals(1, responses.getContent().size());
    Assertions.assertEquals(1, responses.getTotalElements());
    Assertions.assertTrue(responses.getContent().get(0).getValidateOnUi());
    Assertions.assertFalse(responses.getContent().get(0).getValidateByDs());
  }

  @Test
  void upsertRestrictedKeywordTest(){
    RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest =
        new RestrictedKeywordsUpdateRequest(KEYWORD_ID, KEYWORD, true, true);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null,null,true,REQUEST_ID);
    Mockito.when(pcbFeign.updateRestrictedKeyword(restrictedKeywordsUpdateRequest)).thenReturn(response);
    GdnBaseRestResponse  gdnBaseRestResponse = restrictedKeywordService.upsertRestrictedKeyword(restrictedKeywordsUpdateRequest);
    Assertions.assertEquals(true,response.isSuccess());
    Assertions.assertNotNull(gdnBaseRestResponse);
    Mockito.verify(pcbFeign).updateRestrictedKeyword(restrictedKeywordsUpdateRequest);
  }

  @Test
  void upsertRestrictedKeywordWithNullRequestTest(){
    GdnBaseRestResponse  gdnBaseRestResponse = restrictedKeywordService.upsertRestrictedKeyword(null);
    Assertions.assertEquals(false, gdnBaseRestResponse.isSuccess());
    Assertions.assertNotNull(gdnBaseRestResponse);
  }

  @Test
  void getUiValidationRestrictedKeywordsTest() {
    GdnRestListResponse<UiValidationRestrictedKeywordsResponse> responseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(uiValidationRestrictedKeywordsResponse), pageMetaData,
            REQUEST_ID);
    Mockito.when(pcbFeign.getUiValidationRestrictedKeywords()).thenReturn(responseGdnRestListResponse);
    Page<UiValidationRestrictedKeywordsWebResponse> response =
        restrictedKeywordService.getUiValidationRestrictedKeywords();
    Mockito.verify(pcbFeign).getUiValidationRestrictedKeywords();
    Assertions.assertEquals(KEYWORD_ID, response.getContent().get(0).getKeywordId());
  }

  @Test
  void getRestrictedKeywordForListingTest() {
    GdnRestListResponse<RestrictedKeywordsListingResponse> response =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(restrictedKeywordsListingResponse), pageMetaData,
            REQUEST_ID);
    Mockito.when(pcbFeign.getRestrictedKeywordForListing(KEYWORD, page, size)).thenReturn(response);
    Page<RestrictedKeywordsListingWebResponse> restrictedKeywordsListingWebResponses =
        restrictedKeywordService.getRestrictedKeywordForListing(KEYWORD, page, size);
    Mockito.verify(pcbFeign).getRestrictedKeywordForListing(KEYWORD, page, size);
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordsListingWebResponses.getContent().get(0).getKeywordId());
    Assertions.assertEquals(KEYWORD, restrictedKeywordsListingWebResponses.getContent().get(0).getKeyword());
    Assertions.assertTrue(restrictedKeywordsListingWebResponses.getContent().get(0).getValidateOnUi());
  }

  @Test
  void getRestrictedKeywordForListingKeywordNullTest() {
    try {
      restrictedKeywordService.getRestrictedKeywordForListing(null, page, size);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex.getErrorMessage());
    }
  }
}
