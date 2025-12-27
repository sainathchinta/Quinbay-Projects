package com.gda.mta.product.dto;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class MessageEmailRequest {

  private String messageChannel;
  private String messageSubject;
  private String messageCc;
  private String messageBcc;
  private String messageReplyTo;
  private Map<String, String> attachments;
  private List<String> attachmentUrls;
  private String messageId;
  private String messageFrom;
  private String messageTo;
  private String messageIdentifierKey;
  private String messageIdentifierValue;
  private Map<String, String> mainTemplateVariables;
  private Map<String, List<Map<String, String>>> subTemplateVariables;
  private Map<String, Object> variables;
  private boolean enablePublisherConfirm;
  private String storeId;
  private String channelId;
  private String requestId;
  private String clientId;
  private String username;
}
