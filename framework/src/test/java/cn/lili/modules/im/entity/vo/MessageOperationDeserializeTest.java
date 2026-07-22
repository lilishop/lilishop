package cn.lili.modules.im.entity.vo;

import org.junit.jupiter.api.Test;
import tools.jackson.databind.json.JsonMapper;

import static org.assertj.core.api.Assertions.assertThat;

class MessageOperationDeserializeTest {

    private final JsonMapper jsonMapper = JsonMapper.builder().build();

    @Test
    void shouldDeserializeOrderMessageWithObjectContext() throws Exception {
        String payload = """
                {
                  "operation_type": "MESSAGE",
                  "to": "1376433565247471616",
                  "from": "1376417684140326912",
                  "message_type": "ORDER",
                  "context": {
                    "sn": "O2079457050861555713",
                    "groupName": "mate80 白色",
                    "flowPrice": 6999,
                    "orderStatus": "DELIVERED"
                  },
                  "talk_id": "2079823586759684096"
                }
                """;

        MessageOperation messageOperation = jsonMapper.readValue(payload, MessageOperation.class);

        assertThat(messageOperation.getOperationType()).isEqualTo(cn.lili.modules.im.entity.enums.OperationType.MESSAGE);
        assertThat(messageOperation.getMessageType()).isEqualTo(cn.lili.modules.im.entity.enums.MessageTypeEnum.ORDER);
        assertThat(messageOperation.getContext()).contains("O2079457050861555713");
        assertThat(messageOperation.getContext()).contains("mate80 白色");
    }

    @Test
    void shouldDeserializePlainTextContext() throws Exception {
        String payload = """
                {
                  "operation_type": "MESSAGE",
                  "message_type": "MESSAGE",
                  "context": "你好"
                }
                """;

        MessageOperation messageOperation = jsonMapper.readValue(payload, MessageOperation.class);

        assertThat(messageOperation.getContext()).isEqualTo("你好");
    }
}
