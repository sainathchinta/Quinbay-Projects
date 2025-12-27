package com.gdn.x.product.rest.web.controller.api;

import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.TicketTemplateRequest;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleStringResponse;
import com.gdn.x.product.rest.web.model.response.TicketTemplateResponse;
import com.gdn.x.product.service.api.TicketTemplateService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Deprecated
@RequestMapping(value = ProductApiPath.TICKET)
@Tag(name = "Ticket Template Controller", description = "Ticket Template Service API")
public class TicketTemplateController {

  private static final String TICKET_TEMPLATE_NOT_FOUND_WITH_CODE =
      "Ticket template not found with code ";

  @Autowired
  private TicketTemplateService ticketTemplateService;

  @Autowired
  private ModelConverter modelConverter;

  @RequestMapping(value = ProductApiPath.ADD, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "add a new ticket template",
      description = "add a new ticket template, return inserted ticket template's id")
  public GdnRestSingleResponse<SimpleStringResponse> addTicketTemplate(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody TicketTemplateRequest request) throws Exception {
    return new GdnRestSingleResponse<SimpleStringResponse>(new SimpleStringResponse(
        ticketTemplateService.save(storeId,
            modelConverter.convertRequestToModel(request, TicketTemplate.class))), requestId);
  }

  @RequestMapping(value = ProductApiPath.ASSIGN, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "assign ticket template code to selected items",
      description = "assign ticket template code to specified item skus")
  public GdnBaseRestResponse assignItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String ticketTemplateCode, @RequestBody SimpleListStringRequest request)
      throws Exception {
    ticketTemplateService.assignToItem(storeId, request.getValue(), ticketTemplateCode);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.DELETE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete ticket template",
      description = "delete ticket template by ticket template code")
  public GdnBaseRestResponse deleteByTicketTemplateCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleRequestHolder request) throws Exception {
    ticketTemplateService.delete(storeId, request.getId());
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.GET_SUMMARY, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find ticket template",
      description = "find ticket template by ticket template code")
  public GdnRestListResponse<TicketTemplateResponse> findByStoreId(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam String sortBy,
      @RequestParam Direction sortType) throws Exception {
    Page<TicketTemplate> ticketTemplates =
        ticketTemplateService.findByStoreIdAndMarkForDeleteFalse(storeId, PageRequest.of(page,
            size, Sort.by(sortType, sortBy)));
    return new GdnRestListResponse<TicketTemplateResponse>(modelConverter.convertListToResponse(
        ticketTemplates.getContent(), TicketTemplateResponse.class), new PageMetaData(
        ticketTemplates.getSize(), page, ticketTemplates.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_BY_TICKET_TEMPLATE_CODE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find ticket template",
      description = "find ticket template by ticket template code")
  public GdnRestSingleResponse<TicketTemplateResponse> findByTicketTemplateCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String ticketTemplateCode) throws Exception {
    TicketTemplate ticketTemplate =
        ticketTemplateService.findByTicketTemplateCodeAndMarkForDeleteFalse(ticketTemplateCode);
    GdnRestSingleResponse<TicketTemplateResponse> response;
    if (ticketTemplate == null) {
      response =
          new GdnRestSingleResponse<TicketTemplateResponse>(TICKET_TEMPLATE_NOT_FOUND_WITH_CODE
              + ticketTemplateCode, ErrorCategory.DATA_NOT_FOUND.getCode(), false, null, requestId);
    } else {
      response =
          new GdnRestSingleResponse<TicketTemplateResponse>(modelConverter.convertToResponse(
              ticketTemplate, TicketTemplateResponse.class), requestId);
    }
    return response;
  }

  @RequestMapping(value = ProductApiPath.GET_TICKET_TEMPLATE_BY_NAME_LIKE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find ticket template by name",
      description = "find ticket template by ticket template name like. Will only return top 10 data")
  public GdnRestListResponse<TicketTemplateResponse> findByTicketTemplateNameLike(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String name) throws Exception {
    Page<TicketTemplate> ticketTemplate = ticketTemplateService.findByNameLike(storeId, name);
    return new GdnRestListResponse<TicketTemplateResponse>(modelConverter.convertListToResponse(
        ticketTemplate, TicketTemplateResponse.class), new PageMetaData(ticketTemplate.getSize(),
        ticketTemplate.getNumber(), ticketTemplate.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_SKU_BY_TICKET_TEMPLATE_CODE,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find ticket template",
      description = "find ticket template by ticket template code")
  public GdnRestSingleResponse<SimpleListStringResponse> findItemSkuByTicketTemplateCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String ticketTemplateCode) throws Exception {
    List<String> itemSkus =
        ticketTemplateService.findItemSkuByTicketTemplateCode(ticketTemplateCode);
    return new GdnRestSingleResponse<SimpleListStringResponse>(new SimpleListStringResponse(
        itemSkus), requestId);
  }

  @RequestMapping(value = ProductApiPath.UNASSIGN, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "assign ticket template code to selected items",
      description = "assign ticket template code to specified item skus")
  public GdnBaseRestResponse unassignItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String ticketTemplateCode, @RequestBody SimpleListStringRequest request)
      throws Exception {
    ticketTemplateService.unassignToItem(storeId, request.getValue());
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update a ticket template",
      description = "update a ticket template, by ticket template code")
  public GdnBaseRestResponse updateTicketTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody TicketTemplateRequest request) throws Exception {
    ticketTemplateService.update(modelConverter
        .convertRequestToModel(request, TicketTemplate.class));
    return new GdnBaseRestResponse(requestId);
  }
}
