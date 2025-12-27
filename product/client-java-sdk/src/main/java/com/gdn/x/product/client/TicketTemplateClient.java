package com.gdn.x.product.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.Direction;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.TicketTemplateRequest;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleStringResponse;
import com.gdn.x.product.rest.web.model.response.TicketTemplateResponse;

public class TicketTemplateClient extends GdnBaseRestCrudClient {

  private static final String TICKET_TEMPLATE_CODE = "ticketTemplateCode";
  private static final String NAME = "name";

  public TicketTemplateClient(GdnRestClientConfiguration clientConfig) {
    super(clientConfig);
    this.setContextPath(ClientConstants.V1);
  }

  public TicketTemplateClient(String username, String password, String host, Integer port,
      String storeId, String clientId, String channelId) {
    super(username, password, host, port, clientId, channelId, storeId, ClientConstants.V1);
  }

  public GdnRestSingleResponse<SimpleStringResponse> addTicketTemplate(String requestId,
      String username, TicketTemplateRequest ticketTemplateRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_ADD, requestId, additionalParameterMap,
            username);
    return this.invokePostType(uri, ticketTemplateRequest, TicketTemplateRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleStringResponse>>() {});
  }

  public GdnBaseRestResponse assignItemSku(String requestId, String username,
      String ticketTemplateCode, SimpleListStringRequest itemSkus) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TicketTemplateClient.TICKET_TEMPLATE_CODE, ticketTemplateCode);
    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_ASSIGN, requestId, additionalParameterMap,
            username);
    return this.invokePostType(uri, itemSkus, SimpleListStringRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse deleteByTicketTemplateCode(String requestId, String username,
      SimpleRequestHolder ticketTemplateCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_DELETE, requestId, additionalParameterMap,
            username);
    return this.invokePostType(uri, ticketTemplateCode, SimpleRequestHolder.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }


  private URI generateURI(String path, String requestId,
      Map<String, String> additionalParameterMap, String username) throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(),
        this.getClientConfig().getPort(), location,
        this.getMandatoryParameter(this.getRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestSingleResponse<TicketTemplateResponse> getByTicketTemplateCode(String requestId,
      String username, String ticketTemplateCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TicketTemplateClient.TICKET_TEMPLATE_CODE, ticketTemplateCode);

    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_GET_BY_TICKET_TEMPLATE_CODE, requestId,
            additionalParameterMap, username);
    return this.invokeGetSingle(uri, TicketTemplateResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<TicketTemplateResponse> getByTicketTemplateNameLike(String requestId,
      String username, String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TicketTemplateClient.NAME, name);

    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_GET_TICKET_TEMPLATE_BY_NAME_LIKE,
            requestId, additionalParameterMap, username);
    return this.invokeGetSummary(uri, TicketTemplateResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<SimpleListStringResponse> getItemSkuByTicketTemplateCode(
      String requestId, String username, String ticketTemplateCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TicketTemplateClient.TICKET_TEMPLATE_CODE, ticketTemplateCode);

    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_GET_ITEM_SKU_BY_TICKET_TEMPLATE_CODE,
            requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, SimpleListStringResponse.class, ClientConstants.JSON_TYPE);
  }

  private String getRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnRestListResponse<TicketTemplateResponse> getTicketTemplateSummary(String requestId,
      String username, int size, int page, String sortBy, Direction sortType) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ClientConstants.PAGE, Integer.toString(page));
    additionalParameterMap.put(ClientConstants.SIZE, Integer.toString(size));
    additionalParameterMap.put(ClientConstants.SORT_BY, sortBy);
    additionalParameterMap.put(ClientConstants.SORT_TYPE, sortType.toString());

    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_GET_SUMMARY, requestId,
            additionalParameterMap, username);
    return this.invokeGetSummary(uri, TicketTemplateResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnBaseRestResponse unassignItemSku(String requestId, String username,
      String ticketTemplateCode, SimpleListStringRequest itemSkus) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TicketTemplateClient.TICKET_TEMPLATE_CODE, ticketTemplateCode);
    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_UNASSIGN, requestId,
            additionalParameterMap, username);
    return this.invokePostType(uri, itemSkus, SimpleListStringRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse update(String requestId, String username, TicketTemplateRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.TICKET_TEMPLATE_UPDATE, requestId, additionalParameterMap,
            username);
    return this.invokePostType(uri, request, TicketTemplateRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }
}
