package cn.lili.modules.im.entity.enums;

/**
 * 消息的类型
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2022/2/10 16:36
 */
public enum MessageStatusEnum {
    //socket刚打开时发送的消息，这个一般是是刚打开socket链接，进行登录，传入token用
    CONNECT,
    //心跳类型的消息，此种类型的消息只有 type 、 text 两种属性
    HEARTBEAT,
    //用户打开一个对话框，准备跟某人聊天时
    OPEN,
    //客服进行自动回复。客户端发起这种类型请求，则是在拉取对方是否有自动回复，如果有，服务端就会给客户端发送过自动回复的信息
    AUTO_REPLY,
    //正常收发消息沟通，文字、表情等沟通
    MSG,
    //扩展。比如发送商品、发送订单
    EXTEND,
    //系统提示，如提示 对方已离线
    SYSTEM,
    //服务端发送到客户端，用于设置客户端的用户信息。会吧 com.xnx3.yunkefu.core.vo.bean.User 传过去
    SET_USER,
    //结束服务
    CLOSE_SERVICE;

}
