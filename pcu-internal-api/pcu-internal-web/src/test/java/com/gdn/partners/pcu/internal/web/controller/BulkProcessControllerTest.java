package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BulkProcessApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.BulkProcessService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;

/**
 * Created by govind on 05/02/2019 AD.
 */

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class BulkProcessControllerTest extends TestHelper{

  private static final String LANGUAGE = "language";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String SEARCH_KEYWORD = "searchKeyword";
  private static final String SORT_COLUMN = "sortColumn";
  private static final String SORT_ORDER = "asc";
  private static final String STATUS_FILTER = "statusFilter";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private BulkProcessService bulkProcessService;

  @InjectMocks
  private BulkProcessController bulkProcessController;


  private ReviewProductsFilterRequest reviewProductsFilterRequest;

  @BeforeEach
  public void init() {
    mockMvc = MockMvcBuilders.standaloneSetup(bulkProcessController).build();

    reviewProductsFilterRequest =
        ReviewProductsFilterRequest.builder().assignedTo(ASSIGNED_TO).categoryCode(CATEGORY_CODE)
            .searchKeyword(SEARCH_KEYWORD).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .sortColumn(SORT_COLUMN).sortOrder(SORT_ORDER).statusFilter(STATUS_FILTER)
            .timeFilter(TIME_FILTER).build();
  }

  @Test
  public void bulkDownloadScreeningProductsTest() throws Exception{
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);

    MockHttpServletRequestBuilder requestBuilder =
        post(BulkProcessApiPath.BASE_PATH + BulkProcessApiPath.DOWNLOAD_PRODUCTS_SCREENING)
            .param("language", LANGUAGE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(reviewProductsFilterRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.bulkProcessService)
        .bulkDownloadScreeningProducts(Constants.USER_NAME,
            reviewProductsFilterRequest, LANGUAGE);
    verify(clientParameterHelper).getUsername();
  }
}
