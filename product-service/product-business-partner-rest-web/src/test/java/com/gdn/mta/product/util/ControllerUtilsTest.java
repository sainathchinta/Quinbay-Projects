package com.gdn.mta.product.util;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by shripati on 19/03/17.
 */
public class ControllerUtilsTest {

    private static final String KEYWORD = "keyword";
    private static final String PRODUCT_SKU = "productSku";
    private static final String REASON= "reason";
    private static final String ACTIVE= "ACTIVE";
    private static final String SUSPENDED= "SUSPENDED";
    private static final String DESCRIPTION = "description";
    private static final String HYPHEN = "-";
    private static final int PAGE = 0;
    private static final int SIZE = 30;
    private static final String USP_WITH_SPECIAL_CHARACTERS_AND_SPACES =
        "• Case Premium Bagian Belakang Dove Transparan , Kelebihan :~Bagian belakang dove.\n"
            + "~Ada crack / Anti jatuh.\n" + "~Bagian belakang transparan tampil elegant\n"
            + "~Anti Pecah / Sobek karena pakai bahan lentur.\n" + "~Penuh warna glosy di List sekelilingnya\n"
            + "~Slim dan pas di gengam di tangan\n" + "~Anti lecet karena menggunakan Kualitas bahan terbaik\n"
            + "~Lepas pasang sangat mudah , tidak keras / kasar.";
    private static final String DESCRIPTION_WITH_PARAGRAPH_TAGS =
        "<!DOCTYPE html>\n" + "<html>\n" + "<head>\n" + "</head>\n" + "<body>\n"
            + "<p>Character Counter is a 100% free online character count calculator that's simple to use. Sometimes users prefer simplicity over all of the detailed writing information Word Counter provides, and this is exactly what this tool offers. It displays character count and word count which is often the only information a person needs to know about their writing. Best of all, you receive the needed information at a lightning fast speed.</p>\n"
            + "<p>To find out the word and character count of your writing, simply copy and paste text into the tool or write directly into the text area. Once done, the free online tool will display both counts for the text that's been inserted. This can be useful in many instances, but it can be especially helpful when you are writing for something that has a character minimum or limit.</p>\n"
            + "<p>Character and word limits are quite common these days on the Internet. The one that most people are likely aware of is the 140 character limit for tweets on Twitter, but character limits aren't restricted to Twitter. There are limits for text messages (SMS), Yelp reviews, Facebook posts, Pinterest pins, Reddit titles and comments, eBay titles and descriptions as well as many others. Knowing these limits, as well as being able to see as you approach them, will enable you to better express yourself within the imposed limits.</p>\n"
            + "<p>For students, there are usually limits or minimums for homework assignments. The same is often true for college applications. Abiding by these can have a major impact on how this writing is graded and reviewed, and it shows whether or not you're able to follow basic directions. Character counter can make sure you don't accidentally go over limits or fail to meet minimums that can be detrimental to these assignments.</p>\n"
            + "<p>This information can also be quite helpful for writers. Knowing the number of words and characters can help writers better understand the length of their writing, and work to display the pages of their writing in a specific way. For those who write for magazines and newspapers where there is limited space, knowing these counts can help the writer get the most information into that limited space.</p>\n"
            + "<p>For job seekers, knowing the number of characters of your resume can be essential to get all the information you want onto a single page. You can fool around with different fonts, their sizes and spacing to adjust the number of characters you can fit on a single page, but it's important to know the number you're attempting to fit on the page.</p>\n"
            + "<p>Character Counter isn't only for English. The tool can be helpful for those writing in non-English languages where character count is important. This can be the case for languages Japanese, Korean, Chinese and many others where characters are the basis of the written language. Even for those who aren't writing in English, knowing the character count of the writing is often beneficial to the writing.</p>\n"
            + "<p>&nbsp;</p>\n"
            + "<p><br />Character Counter isn't only for English. The tool can be helpful for those writing in non-English languages where character count is important. This can be the case for languages Japanese, Korean, Chinese and many others where characters are the basis of the written language. Even for those who aren't writing in English, knowing the character count of the writing is often beneficial to the writing.</p>\n"
            + "<p>Character Counter isn't only for English. The tool can be helpful for those writing in non-English languages where character count is important. This can be the case for languages Japanese, Korean, Chinese and many others where characters are the basis of the written language. Even for those who aren't writing in English, knowing the character count of the writing is often beneficial to the writing.</p>\n"
            + "<p>Character Counter isn't only for English. The tool can be helpful for those writing in non-English languages where character count is important. This can be the case for languages Japanese, Korean, Chinese and many others where characters are the basis of the written language. Even for those who aren't writing in English, knowing the character count of the writing is often beneficial to the writing.</p>\n"
            + "<p>Character Counter isn't only for English. The tool can be helpful for those writing in non-English languages where character count is important. This can be the case for languages Japanese, Korean, Chinese and many others where characters are the basis of the written language. Even for those who aren't writing in English, knowing the character count of the writing is often beneficial to the writing.This information can also be quite helpful for writers. Knowing the number of words and characters can help writers better understand the length of their writing, and work to display the pages of their writing in a specific way. For those who write for magazines and newspapers where there is limited space, knowing these 1234cou including paragraphs</p>\n"
            + "</body>\n" + "</html>";

    private ProductSuspensionHistory productSuspensionHistory = new ProductSuspensionHistory();

    @BeforeEach
    public void init(){
        productSuspensionHistory.setProductSku(PRODUCT_SKU);
        productSuspensionHistory.setReason(REASON);
        productSuspensionHistory.setDescription(DESCRIPTION);
    }

    @Test public void test_convertItemResponseToItemRequest() {
        ItemResponse expected = new ItemResponse();
        expected.setMerchantSku("12345");
        expected.setItemSku("TOA-14567-12131-00001");
        expected.setItemCode("TOA-14567-12131");
        expected.setProductSku("MTA-12345");
        expected.setMasterDataItem(new MasterDataItemDTO());
        Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
        itemViewConfigs.add(new ItemViewConfigDTO());
        expected.setItemViewConfigs(itemViewConfigs);
        expected.setLateFulfillment(true);
        expected.setPickupPointCode("PP-21232");

        ItemRequest actual = ControllerUtils.convertItemResponseToItemRequest(expected);
        Assertions.assertEquals(expected.getMerchantSku(), actual.getMerchantSku());
        Assertions.assertEquals(expected.getItemSku(), actual.getItemSku());
        Assertions.assertEquals(expected.getProductSku(), actual.getProductSku());
        Assertions.assertEquals(expected.getItemCode(), actual.getItemCode());
        Assertions.assertEquals(expected.getPickupPointCode(), actual.getPickupPointCode());
        Assertions.assertEquals(expected.isLateFulfillment(), actual.isLateFulfillment());
    }

    @Test
    public void getSummaryFilterServiceeRequestTest() {
        SummaryFilterRequest request = SummaryFilterRequest
            .builder().statusFilter(SolrConstants.ALL).timeFilter(SolrConstants.ALL)
            .searchKeyword(KEYWORD).build();
        SummaryFilterServiceRequest summaryFilterServiceRequest = ControllerUtils
            .getSummaryFilterServiceRequest(request);
        Assertions.assertNotNull(summaryFilterServiceRequest);
        Assertions.assertEquals(KEYWORD, summaryFilterServiceRequest.getSearchKeyword());
    }

    @Test
    public void convertToProductSuspensionHistoryResponseActiveStatusTest() {
        productSuspensionHistory.setStatus(SuspensionStatus.ACTIVE);
        List<ProductSuspensionHistoryResponse> result = ControllerUtils
            .convertProductSuspensionHistoryToProductSuspensionHistoryResponse(
                Collections.singletonList(productSuspensionHistory));
        Assertions.assertEquals(PRODUCT_SKU, result.get(0).getProductSku());
        Assertions.assertEquals(ACTIVE, result.get(0).getStatus());
        Assertions.assertEquals(DESCRIPTION + HYPHEN + REASON, result.get(0).getReason());
    }

    @Test
    public void convertToProductSuspensionHistoryResponseSuspendedStatusTest() {
        productSuspensionHistory.setStatus(SuspensionStatus.SUSPENDED);
        List<ProductSuspensionHistoryResponse> result = ControllerUtils
            .convertProductSuspensionHistoryToProductSuspensionHistoryResponse(
                Collections.singletonList(productSuspensionHistory));
        Assertions.assertEquals(PRODUCT_SKU, result.get(0).getProductSku());
        Assertions.assertEquals(SUSPENDED, result.get(0).getStatus());
        Assertions.assertEquals(DESCRIPTION + HYPHEN + REASON, result.get(0).getReason());
    }

    @Test
    public void getFilterUSPTest() {
        String filterUSP = ControllerUtils.getFilteredUSPAndDescription(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES);
        Assertions.assertTrue(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES.length() > filterUSP.length());
    }

    @Test
    public void getFilterDescriptionTest() {
        String filterDescription = ControllerUtils.getDescriptionWithoutParagraphTags(DESCRIPTION_WITH_PARAGRAPH_TAGS);
        Assertions.assertTrue(DESCRIPTION_WITH_PARAGRAPH_TAGS.length() > filterDescription.length());
    }
}
