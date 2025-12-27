package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gdn.mta.product.util.BeanUtils;

import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.RecategorizationRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import org.apache.commons.lang3.StringUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.valueobject.BulkMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
/**
 * Created by riteshkumar on 23/02/17.
 */
public class ControllerUtils {

  private static final String HYPHEN = "-";

  private static final String REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE = "([^\\x00-\\x7F])|(/ +/ig)";
  private static final String REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      "(\\<.*?\\>|&\\w+.;)|(/\\r\\n|\\n|\\r/gm)|([\\•|\\)]\\s+)";
  private static final Pattern PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      Pattern.compile(REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT);
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE =
      Pattern.compile(REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE);
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);
  private static final String REGEX_FOR_OPENING_PARAGRAPH_TAGS = "<p>";
  private static final String REGEX_FOR_CLOSING_PARAGRAPH_TAGS = "</p>";
  private static final Pattern PATTERN_FOR_OPENING_PARAGRAPH_TAG = Pattern.compile(REGEX_FOR_OPENING_PARAGRAPH_TAGS);
  private static final Pattern PATTERN_FOR_CLOSING_PARAGRAPH_TAG = Pattern.compile(REGEX_FOR_CLOSING_PARAGRAPH_TAGS);
  private static final String REGEX_FOR_REMOVE_LAST_NEW_LINE = "[\n\r]$";
  private static final Pattern PATTERN_FOR_REMOVE_LAST_NEW_LINE = Pattern.compile(REGEX_FOR_REMOVE_LAST_NEW_LINE);
  private static final String NEW_LINE = "\n";

  public static ItemRequest convertItemResponseToItemRequest(ItemResponse itemResponse) {
        ItemRequest itemRequest = new ItemRequest();
        itemRequest.setItemSku(itemResponse.getItemSku());
        itemRequest.setItemCode(itemResponse.getItemCode());
        itemRequest.setProductSku(itemResponse.getProductSku());
        itemRequest.setMerchantSku(itemResponse.getMerchantSku());
        itemRequest.setMasterDataItem(itemResponse.getMasterDataItem());
        Set<ItemViewConfigRequest> itemViewConfigRequests = new HashSet<>();
        for (ItemViewConfigDTO itemViewConfigDTO : itemResponse.getItemViewConfigs()) {
            ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest(
                itemViewConfigDTO.isBuyable(), itemViewConfigDTO.isDiscoverable(),
                itemViewConfigDTO.getChannel(), itemViewConfigDTO.getItemDiscoverableSchedules(),
                itemViewConfigDTO.getItemBuyableSchedules());
            itemViewConfigRequests.add(itemViewConfigRequest);
        }
        itemRequest.setItemViewConfigs(itemViewConfigRequests);
        itemRequest.setLateFulfillment(itemResponse.isLateFulfillment());
        itemRequest.setPickupPointCode(itemResponse.getPickupPointCode());
        return itemRequest;
    }

  public static void validateRecategorizationRequest(RecategorizationRequest request) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getName()), Constants.REQUIRED_REACT_NAME);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getExcelFilePath()), Constants.REQUIRED_FILE_PATH);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getId()), Constants.REQUIRED_ID);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getStatus()), Constants.REQUIRED_STATUS);
  }

  private static SimpleMasterProductUpdateRequestDTO getSimpleMasterProductUpdateRequestDTO(
      SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest, String updatedBy, Date updatedDate){
    SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO = new SimpleMasterProductUpdateRequestDTO();
    BeanUtils.copyProperties(simpleMasterProductUpdateRequest, simpleMasterProductUpdateRequestDTO);
    simpleMasterProductUpdateRequestDTO.setUpdatedBy(updatedBy);
    simpleMasterProductUpdateRequestDTO.setUpdatedDate(updatedDate);
    return simpleMasterProductUpdateRequestDTO;
  }

  public static BulkMasterProductUpdateRequestDTO getBulkMasterProductUpdateRequestDTO(
      BulkMasterProductUpdateRequest bulkMasterProductUpdateRequest){
    List<SimpleMasterProductUpdateRequestDTO> simpleMasterProductUpdateRequestDTOList =
        bulkMasterProductUpdateRequest.getSimpleMasterProductUpdateRequests().stream()
            .map(simpleMasterProductUpdateRequest -> ControllerUtils.getSimpleMasterProductUpdateRequestDTO(
                simpleMasterProductUpdateRequest,bulkMasterProductUpdateRequest.getUpdatedBy(),
                bulkMasterProductUpdateRequest.getUpdatedDate())).collect(Collectors.toList());
    return new BulkMasterProductUpdateRequestDTO(simpleMasterProductUpdateRequestDTOList);
  }

  public static SummaryFilterServiceRequest getSummaryFilterServiceRequest(SummaryFilterRequest request) {
    String searchKeyword =
        StringUtils.isBlank(request.getSearchKeyword()) ? StringUtils.EMPTY : request.getSearchKeyword();
    return SummaryFilterServiceRequest.builder()
        .statusFilter(StatusFilterType.getStatusFilterTypeByValue(request.getStatusFilter()))
        .timeFilter(TimeFilterType.getTimeFilterTypeByValue(request.getTimeFilter())).searchKeyword(searchKeyword)
        .build();
  }

  public static List<ProductSuspensionHistoryResponse> convertProductSuspensionHistoryToProductSuspensionHistoryResponse(
      List<ProductSuspensionHistory> productSuspensionHistories) {
    List<ProductSuspensionHistoryResponse> productSuspensionHistoryResponseList = new ArrayList<>();
    for (ProductSuspensionHistory history : productSuspensionHistories) {
      ProductSuspensionHistoryResponse wrapper = new ProductSuspensionHistoryResponse();
      BeanUtils.copyProperties(history, wrapper);
      StringBuilder description = new StringBuilder(history.getDescription());
      description.append(HYPHEN).append(history.getReason());
      wrapper.setReason(description.toString());
      if(SuspensionStatus.SUSPENDED.equals(history.getStatus())){
        wrapper.setStatus(SuspensionStatus.SUSPENDED.toString());
      } else {
        wrapper.setStatus(SuspensionStatus.ACTIVE.toString());
      }
      productSuspensionHistoryResponseList.add(wrapper);
    }
    return productSuspensionHistoryResponseList;
  }

  public static String getFilteredUSPAndDescription(String uniqueSellingPoint) {
    String uspWithoutTagsAndNewLine = getTextWithoutTags(PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE,
        getTextWithoutTags(PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT, uniqueSellingPoint, StringUtils.EMPTY),
        StringUtils.EMPTY);
    return getTextWithoutTags(PATTERN_FOR_EXTRA_SPACE, uspWithoutTagsAndNewLine, StringUtils.SPACE);
  }

  public static String getDescriptionWithoutParagraphTags(String description) {
    String descriptionWithoutTags = getTextWithoutTags(PATTERN_FOR_CLOSING_PARAGRAPH_TAG,
        getTextWithoutTags(PATTERN_FOR_OPENING_PARAGRAPH_TAG, description, StringUtils.EMPTY), NEW_LINE);
    return getTextWithoutTags(PATTERN_FOR_REMOVE_LAST_NEW_LINE, descriptionWithoutTags, StringUtils.EMPTY);
  }

  private static String getTextWithoutTags(Pattern pattern, String usp, String replace) {
    return pattern.matcher(usp).replaceAll(replace);
  }
}
