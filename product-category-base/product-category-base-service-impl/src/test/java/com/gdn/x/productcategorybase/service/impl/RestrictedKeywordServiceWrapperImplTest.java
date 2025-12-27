package com.gdn.x.productcategorybase.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.RestrictedKeywordActivity;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class RestrictedKeywordServiceWrapperImplTest {

    @Mock
    private RestrictedKeywordService restrictedKeywordService;

    @Mock
    private KafkaPublisher kafkaProducer;

    @InjectMocks
    private RestrictedKeywordServiceWrapperImpl restrictedKeywordServiceWrapper;

    private RestrictedKeyword restrictedKeyword;
    private RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO;

    private static final String STORE_ID = "10001";
    private static final String KEYWORD_ID = "keywordId";
    private static final String KEYWORD = "keyword";
    private static final String KEYWORD_1 = "keyword_1";

    @BeforeEach
    public void setUp() throws Exception {
        initMocks(this);
        restrictedKeyword = new RestrictedKeyword();
        restrictedKeyword.setKeyword(KEYWORD);
        restrictedKeyword.setId(KEYWORD_ID);
        restrictedKeyword.setValidateByDs(Boolean.TRUE);
        restrictedKeyword.setValidateOnUi(Boolean.FALSE);
        restrictedKeyword.setStoreId(STORE_ID);

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).validateOnUi(true)
                .validateByDs(false).build();
    }

    @Test
    public void updateRestrictedKeywordsTest_NewKeywordAddedCase1() throws Exception {
        //keyword add operation for completely new entry in table
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(null);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        // Activity history for keyword add operation
        RestrictedKeywordActivityHistory restrictedKeywordActivityHistory = new RestrictedKeywordActivityHistory();
        restrictedKeywordActivityHistory.setActivity(RestrictedKeywordActivity.ADD.getDescription());
        restrictedKeywordActivityHistory.setOldValue("");
        restrictedKeywordActivityHistory.setNewValue("");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordActivityHistory);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_NewKeywordAddedCase2() throws Exception {
        //keyword add operation for a deleted keyword in table (update deleted keyword)
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeyword = new RestrictedKeyword();
        restrictedKeyword.setKeyword(KEYWORD);
        restrictedKeyword.setId(KEYWORD_ID);
        restrictedKeyword.setValidateOnUi(null);
        restrictedKeyword.setValidateByDs(null);
        restrictedKeyword.setStoreId(STORE_ID);

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        // Activity history for keyword add operation
        RestrictedKeywordActivityHistory restrictedKeywordActivityHistory = new RestrictedKeywordActivityHistory();
        restrictedKeywordActivityHistory.setActivity(RestrictedKeywordActivity.ADD.getDescription());
        restrictedKeywordActivityHistory.setOldValue("");
        restrictedKeywordActivityHistory.setNewValue("");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordActivityHistory);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_FlagUpdatedCase1() throws Exception {
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD_1).validateOnUi(true)
                .validateByDs(false).build();

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        RestrictedKeywordActivityHistory restrictedKeywordValidateOnUiChangeActivityHistory =
            new RestrictedKeywordActivityHistory();
        restrictedKeywordValidateOnUiChangeActivityHistory.setActivity(
            RestrictedKeywordActivity.UI_VALIDATION_UPDATED.getDescription());
        restrictedKeywordValidateOnUiChangeActivityHistory.setOldValue("false");
        restrictedKeywordValidateOnUiChangeActivityHistory.setNewValue("true");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordValidateOnUiChangeActivityHistory);

        RestrictedKeywordActivityHistory restrictedKeywordValidateByDsChangeActivityHistory =
            new RestrictedKeywordActivityHistory();
        restrictedKeywordValidateByDsChangeActivityHistory.setActivity(
            RestrictedKeywordActivity.DS_VALIDATION_UPDATED.getDescription());
        restrictedKeywordValidateByDsChangeActivityHistory.setOldValue("true");
        restrictedKeywordValidateByDsChangeActivityHistory.setNewValue("false");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordValidateByDsChangeActivityHistory);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_FlagUpdatedCase2() throws Exception {
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeyword = new RestrictedKeyword();
        restrictedKeyword.setKeyword(KEYWORD);
        restrictedKeyword.setId(KEYWORD_ID);
        restrictedKeyword.setValidateOnUi(Boolean.TRUE);
        restrictedKeyword.setValidateByDs(Boolean.FALSE);
        restrictedKeyword.setStoreId(STORE_ID);

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).validateOnUi(false)
                .validateByDs(true).build();

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        RestrictedKeywordActivityHistory restrictedKeywordValidateOnUiChangeActivityHistory =
            new RestrictedKeywordActivityHistory();
        restrictedKeywordValidateOnUiChangeActivityHistory.setActivity(
            RestrictedKeywordActivity.UI_VALIDATION_UPDATED.getDescription());
        restrictedKeywordValidateOnUiChangeActivityHistory.setOldValue("true");
        restrictedKeywordValidateOnUiChangeActivityHistory.setNewValue("false");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordValidateOnUiChangeActivityHistory);

        RestrictedKeywordActivityHistory restrictedKeywordValidateByDsChangeActivityHistory =
            new RestrictedKeywordActivityHistory();
        restrictedKeywordValidateByDsChangeActivityHistory.setActivity(
            RestrictedKeywordActivity.DS_VALIDATION_UPDATED.getDescription());
        restrictedKeywordValidateByDsChangeActivityHistory.setOldValue("false");
        restrictedKeywordValidateByDsChangeActivityHistory.setNewValue("true");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordValidateByDsChangeActivityHistory);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_KeywordDelete() throws Exception {
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).validateOnUi(null)
                .validateByDs(null).build();

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        RestrictedKeywordActivityHistory restrictedKeywordDeleteKeywordActivityHistory =
            new RestrictedKeywordActivityHistory();
        restrictedKeywordDeleteKeywordActivityHistory.setActivity(RestrictedKeywordActivity.DELETE.getDescription());
        restrictedKeywordDeleteKeywordActivityHistory.setOldValue("");
        restrictedKeywordDeleteKeywordActivityHistory.setNewValue("");
        restrictedKeywordActivityHistoryList.add(restrictedKeywordDeleteKeywordActivityHistory);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_FlagUpdateCase3() throws Exception {
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeyword = new RestrictedKeyword();
        restrictedKeyword.setKeyword(KEYWORD);
        restrictedKeyword.setId(KEYWORD_ID);
        restrictedKeyword.setValidateOnUi(Boolean.TRUE);
        restrictedKeyword.setValidateByDs(null);
        restrictedKeyword.setStoreId(STORE_ID);

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).validateOnUi(Boolean.FALSE)
                .validateByDs(null).build();

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        RestrictedKeywordActivityHistory validateOnUiActivity = new RestrictedKeywordActivityHistory();
        validateOnUiActivity.setActivity(RestrictedKeywordActivity.UI_VALIDATION_UPDATED.getDescription());
        validateOnUiActivity.setOldValue("true");
        validateOnUiActivity.setNewValue("false");
        restrictedKeywordActivityHistoryList.add(validateOnUiActivity);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTest_FlagUpdateCase4() throws Exception {
        RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
            new RestrictedKeywordHistoryEventModel();
        List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

        restrictedKeyword = new RestrictedKeyword();
        restrictedKeyword.setKeyword(KEYWORD);
        restrictedKeyword.setId(KEYWORD_ID);
        restrictedKeyword.setValidateOnUi(null);
        restrictedKeyword.setValidateByDs(Boolean.TRUE);
        restrictedKeyword.setStoreId(STORE_ID);

        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).validateOnUi(null)
                .validateByDs(Boolean.FALSE).build();
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(List.of(restrictedKeyword));

        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);

        RestrictedKeywordActivityHistory validateByDsActivity = new RestrictedKeywordActivityHistory();
        validateByDsActivity.setActivity(RestrictedKeywordActivity.DS_VALIDATION_UPDATED.getDescription());
        validateByDsActivity.setOldValue("true");
        validateByDsActivity.setNewValue("false");
        restrictedKeywordActivityHistoryList.add(validateByDsActivity);

        restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
            restrictedKeywordActivityHistoryList);
        restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
        restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
        restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());

        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);

        Mockito.verify(kafkaProducer).send(anyString(), anyString(), eq(restrictedKeywordHistoryEventModel));
        Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
        Mockito.verify(this.restrictedKeywordService)
            .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
    }

    @Test
    public void updateRestrictedKeywordsTestEmptyKeyword() {
        restrictedKeywordsUpdateDTO =
            RestrictedKeywordsUpdateDTO.builder().keywordId(KEYWORD_ID).keyword("").validateOnUi(true)
                .validateByDs(false).build();
        restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID);
        Mockito.verifyNoInteractions(kafkaProducer, restrictedKeywordService);
    }

    @Test
    public void updateRestrictedKeywordsTestEmptyKeywordList() {
        Mockito.when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(),
            anyString())).thenReturn(restrictedKeyword);
        Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(anyList()))
            .thenReturn(Collections.emptyList());
        try {
            Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, STORE_ID));
        } finally {
            Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(anyList());
            Mockito.verify(this.restrictedKeywordService)
                .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(anyString(), anyString());
            Mockito.verifyNoInteractions(kafkaProducer);
        }
    }

    @AfterEach
    public void tearDown() {
        verifyNoMoreInteractions(restrictedKeywordService, kafkaProducer);
    }
}
