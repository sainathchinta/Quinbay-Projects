package com.gdn.x.productcategorybase.service.impl;

import static com.gdn.x.productcategorybase.domain.event.config.DomainEventName.RESTRICTED_KEYWORD_HISTORY_EVENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.RestrictedKeywordActivity;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordServiceWrapper;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordServiceWrapperImpl implements RestrictedKeywordServiceWrapper {

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;
  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Override
  public void updateRestrictedKeyword(RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO, String storeId) {
    if (StringUtils.isNotEmpty(restrictedKeywordsUpdateDTO.getKeyword())) {
      RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
          saveRestrictedKeywords(restrictedKeywordsUpdateDTO, storeId);
      kafkaPublisher.send(RESTRICTED_KEYWORD_HISTORY_EVENT, restrictedKeywordsUpdateDTO.getKeywordId(),
          restrictedKeywordHistoryEventModel);
    } else {
      log.error("Exception occurred while updating restricted keywords : {} ", restrictedKeywordsUpdateDTO);
    }
  }

  private RestrictedKeywordHistoryEventModel saveRestrictedKeywords(
      RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO, String storeId) {
    RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel = new RestrictedKeywordHistoryEventModel();
    List<RestrictedKeywordActivityHistory> restrictedKeywordActivityHistoryList = new ArrayList<>();

    RestrictedKeyword savedKeyword =
        restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(storeId,
            restrictedKeywordsUpdateDTO.getKeyword());
    savedKeyword = Optional.ofNullable(savedKeyword).orElse(new RestrictedKeyword());
    /*
    Add operation will occur in following two cases:
    1. savedKeyword does not exist in db i.e. savedKeyword is null
    2. deleted keyword is updated in the db
    */
    if (Objects.isNull(savedKeyword.getValidateOnUi()) && Objects.isNull(savedKeyword.getValidateByDs())) {
      savedKeyword.setStoreId(storeId);
      savedKeyword.setKeyword(restrictedKeywordsUpdateDTO.getKeyword());
      savedKeyword.setValidateOnUi(restrictedKeywordsUpdateDTO.getValidateOnUi());
      savedKeyword.setValidateByDs(restrictedKeywordsUpdateDTO.getValidateByDs());
      // Activity history for keyword add operation
      RestrictedKeywordActivityHistory addActivity = new RestrictedKeywordActivityHistory();
      addActivity.setActivity(RestrictedKeywordActivity.ADD.getDescription());
      addActivity.setOldValue(StringUtils.EMPTY);
      addActivity.setNewValue(StringUtils.EMPTY);
      restrictedKeywordActivityHistoryList.add(addActivity);
    } else {
      if (Objects.isNull(restrictedKeywordsUpdateDTO.getValidateOnUi()) && Objects.isNull(
          restrictedKeywordsUpdateDTO.getValidateByDs())) {
        savedKeyword.setValidateByDs(null);
        savedKeyword.setValidateOnUi(null);
        // Activity history for keyword delete operation
        RestrictedKeywordActivityHistory deleteActivity = new RestrictedKeywordActivityHistory();
        deleteActivity.setActivity(RestrictedKeywordActivity.DELETE.getDescription());
        deleteActivity.setOldValue(StringUtils.EMPTY);
        deleteActivity.setNewValue(StringUtils.EMPTY);
        restrictedKeywordActivityHistoryList.add(deleteActivity);
      } else {
        if (savedKeyword.getValidateOnUi() != restrictedKeywordsUpdateDTO.getValidateOnUi()) {
          savedKeyword.setValidateOnUi(restrictedKeywordsUpdateDTO.getValidateOnUi());
          // Activity history for keyword validateOnUi operation
          RestrictedKeywordActivityHistory validateOnUiActivity = new RestrictedKeywordActivityHistory();
          validateOnUiActivity.setActivity(RestrictedKeywordActivity.UI_VALIDATION_UPDATED.getDescription());
          if ((restrictedKeywordsUpdateDTO.getValidateOnUi())) {
            validateOnUiActivity.setOldValue(Boolean.FALSE.toString());
            validateOnUiActivity.setNewValue(Boolean.TRUE.toString());
          } else {
            validateOnUiActivity.setOldValue(Boolean.TRUE.toString());
            validateOnUiActivity.setNewValue(Boolean.FALSE.toString());
          }
          restrictedKeywordActivityHistoryList.add(validateOnUiActivity);
        }
        if (savedKeyword.getValidateByDs() != restrictedKeywordsUpdateDTO.getValidateByDs()) {
          savedKeyword.setValidateByDs(restrictedKeywordsUpdateDTO.getValidateByDs());
          // Activity history for keyword validateOnDs operation
          RestrictedKeywordActivityHistory validateByDsActivity = new RestrictedKeywordActivityHistory();
          validateByDsActivity.setActivity(RestrictedKeywordActivity.DS_VALIDATION_UPDATED.getDescription());
          if ((restrictedKeywordsUpdateDTO.getValidateByDs())) {
            validateByDsActivity.setOldValue(Boolean.FALSE.toString());
            validateByDsActivity.setNewValue(Boolean.TRUE.toString());
          } else {
            validateByDsActivity.setOldValue(Boolean.TRUE.toString());
            validateByDsActivity.setNewValue(Boolean.FALSE.toString());
          }
          restrictedKeywordActivityHistoryList.add(validateByDsActivity);
        }
      }
    }
    log.info("Saving the restrictedKeywordsToSave : {}", savedKeyword);
    List<RestrictedKeyword> keywords =
        restrictedKeywordService.saveRestrictedKeywords(Collections.singletonList(savedKeyword));
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(keywords),
        ErrorMessage.RESTRICTED_KEYWORD_ERROR.getMessage());
    restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(restrictedKeywordActivityHistoryList);
    restrictedKeywordHistoryEventModel.setKeywordId(keywords.stream().map(GdnBaseEntity::getId).findAny().orElse(StringUtils.EMPTY));
    restrictedKeywordHistoryEventModel.setStoreId(keywords.stream().map(GdnBaseEntity::getStoreId).findAny().orElse(StringUtils.EMPTY));
    restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());
    return restrictedKeywordHistoryEventModel;
  }
}
