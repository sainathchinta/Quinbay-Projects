package com.gdn.x.productcategorybase.dto.mail;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by Vishal on 04/05/18.
 */
public class MailRecipientRequest implements Serializable{
  private static final long serialVersionUID = 2612954107124916440L;

  private String emailCc;
  private String emailTo;

  public MailRecipientRequest(String emailCc, String emailTo) {
    this.emailCc = emailCc;
    this.emailTo = emailTo;
  }

  public MailRecipientRequest() {
  }

  public String getEmailCc() {
    return emailCc;
  }

  public void setEmailCc(String emailCc) {
    this.emailCc = emailCc;
  }

  public String getEmailTo() {
    return emailTo;
  }

  public void setEmailTo(String emailTo) {
    this.emailTo = emailTo;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("emailCc", emailCc).append("emailTo", emailTo)
        .toString();
  }
}
