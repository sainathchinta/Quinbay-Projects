package com.gdn.partners.pcu.master.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.Arrays;

import com.gdn.partners.pcu.master.web.model.request.RestrictedKeywordsSearchWebRequest;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.RestrictedKeywordApiPath;
import com.gdn.partners.pcu.master.service.RestrictedKeywordService;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;

@ExtendWith(MockitoExtension.class)
public class RestrictedKeywordControllerTest extends TestHelper {

  @InjectMocks
  private RestrictedKeywordController restrictedKeywordController;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private RestrictedKeywordsSearchWebRequest request;

  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD = "keyword";
  private static final int page = 0;
  private static final int size = 10;
  private static final String PAGE = "page";
  private static final String SIZE = "size";

  @BeforeEach
  void setUp() {
    this.mockMvc = standaloneSetup(this.restrictedKeywordController).build();
    request = new RestrictedKeywordsSearchWebRequest();
    request.setKeyword("keyword");
  }

  @AfterEach
  void tearDown() {
    verifyNoMoreInteractions(restrictedKeywordService);
    verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void getRestrictedKeywordHistoryTest() throws Exception {
    RestrictedKeywordHistoryWebResponse response = new RestrictedKeywordHistoryWebResponse();
    response.setKeywordId(KEYWORD_ID);
    Page<RestrictedKeywordHistoryWebResponse> responses =
        new PageImpl<>(Arrays.asList(response), PageRequest.of(page, size), 1);
    Mockito.when(restrictedKeywordService.getRestrictedKeywordHistory(KEYWORD_ID, page, size)).thenReturn(responses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.HISTORY, KEYWORD_ID).contentType(
                MediaType.APPLICATION_JSON).param(PAGE, String.valueOf(page)).param(SIZE, String.valueOf(size))
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordService).getRestrictedKeywordHistory(KEYWORD_ID, page, size);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getRestrictedKeywordsTest() throws Exception {
    RestrictedKeywordsWebResponse response = new RestrictedKeywordsWebResponse();
    response.setKeywordId(KEYWORD);
    response.setValidateByDs(Boolean.FALSE);
    response.setValidateOnUi(Boolean.TRUE);
    Page<RestrictedKeywordsWebResponse> responses =
      new PageImpl<>(Arrays.asList(response), PageRequest.of(page, size), 1);
    Mockito.when(restrictedKeywordService.getRestrictedKeywords(request, page, size))
      .thenReturn(responses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
      post(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.SEARCH).contentType(
          MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
        .param(PAGE, String.valueOf(page)).param(SIZE, String.valueOf(size))
        .content(toJson(request));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordService).getRestrictedKeywords(request, page, size);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  void upsertRestrictedKeywordTest() throws Exception {
    RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest =
        new RestrictedKeywordsUpdateRequest(KEYWORD_ID, KEYWORD, true, true);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, clientParameterHelper.getRequestId());
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(restrictedKeywordService.upsertRestrictedKeyword(restrictedKeywordsUpdateRequest)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        post(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.UPSERT)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(restrictedKeywordsUpdateRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordService).upsertRestrictedKeyword(restrictedKeywordsUpdateRequest);
    Mockito.verify(clientParameterHelper, Mockito.times(2)).getRequestId();
  }

  @Test
  void getUiValidationRestrictedKeywords() throws Exception {
    UiValidationRestrictedKeywordsWebResponse response = new UiValidationRestrictedKeywordsWebResponse();
    response.setKeywordId(KEYWORD_ID);
    Page<UiValidationRestrictedKeywordsWebResponse> responses =
        new PageImpl<>(Arrays.asList(response), PageRequest.of(page, size), 1);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(restrictedKeywordService.getUiValidationRestrictedKeywords()).thenReturn(responses);
    MockHttpServletRequestBuilder requestBuilder =
        get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.UI_VALIDATION).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordService).getUiValidationRestrictedKeywords();
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getRestrictedKeywordForListingTest() throws Exception {
    RestrictedKeywordsListingWebResponse response = new RestrictedKeywordsListingWebResponse();
    response.setKeywordId(KEYWORD_ID);
    response.setKeyword(KEYWORD);
    Page<RestrictedKeywordsListingWebResponse> responses =
        new PageImpl<>(Arrays.asList(response), PageRequest.of(page, size), 1);
    Mockito.when(restrictedKeywordService.getRestrictedKeywordForListing(KEYWORD, page, size)).thenReturn(responses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(RestrictedKeywordApiPath.BASE_PATH + RestrictedKeywordApiPath.LISTING).contentType(
                MediaType.APPLICATION_JSON).param(KEYWORD, KEYWORD).param(PAGE, String.valueOf(page))
            .param(SIZE, String.valueOf(size)).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(restrictedKeywordService).getRestrictedKeywordForListing(KEYWORD, page, size);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

}
