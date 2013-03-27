/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim:set ts=2 sw=2 sts=2 et cindent: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#pragma once

#include "nsCOMPtr.h"
#include "nsDOMEventTargetHelper.h"
#include "nsString.h"
#include "nsWrapperCache.h"
#include "nsIDOMNavigatorUserMedia.h"
#include "nsTArray.h"

#include "MediaManager.h"
#include "MediaEngine.h"
#include "MediaStreamGraph.h"
#include "AudioSegment.h"
#include "mozilla/WeakPtr.h"

#include "EnableWebSpeechRecognitionCheck.h"
#include "SpeechGrammarList.h"
#include "SpeechRecognitionResultList.h"
#include "nsISpeechRecognitionService.h"
#include "endpointer.h"

#include "nsIDOMSpeechRecognitionError.h"

struct JSContext;
class nsIDOMWindow;

namespace mozilla {

namespace dom {

class GlobalObject;
class SpeechEvent;

#ifdef PR_LOGGING
PRLogModuleInfo* GetSpeechRecognitionLog();
#define SR_LOG(...) PR_LOG(GetSpeechRecognitionLog(), PR_LOG_DEBUG, (__VA_ARGS__))
#else
#define SR_LOG(...)
#endif

class SpeechRecognition MOZ_FINAL : public nsDOMEventTargetHelper,
                                    public nsIObserver,
                                    public EnableWebSpeechRecognitionCheck,
                                    public SupportsWeakPtr<SpeechRecognition>
{
public:
  SpeechRecognition();
  virtual ~SpeechRecognition() {};

  NS_DECL_ISUPPORTS_INHERITED

  NS_DECL_NSIOBSERVER

  nsISupports* GetParentObject() const;

  virtual JSObject* WrapObject(JSContext* aCx, JSObject* aScope);

  static already_AddRefed<SpeechRecognition> Constructor(const GlobalObject& aGlobal, ErrorResult& aRv);

  already_AddRefed<SpeechGrammarList> GetGrammars(ErrorResult& aRv) const;

  void SetGrammars(mozilla::dom::SpeechGrammarList& aArg, ErrorResult& aRv);

  void GetLang(nsString& aRetVal, ErrorResult& aRv) const;

  void SetLang(const nsAString& aArg, ErrorResult& aRv);

  bool GetContinuous(ErrorResult& aRv) const;

  void SetContinuous(bool aArg, ErrorResult& aRv);

  bool GetInterimResults(ErrorResult& aRv) const;

  void SetInterimResults(bool aArg, ErrorResult& aRv);

  uint32_t GetMaxAlternatives(ErrorResult& aRv) const;

  void SetMaxAlternatives(uint32_t aArg, ErrorResult& aRv);

  void GetServiceURI(nsString& aRetVal, ErrorResult& aRv) const;

  void SetServiceURI(const nsAString& aArg, ErrorResult& aRv);

  void Start(ErrorResult& aRv);

  void Stop();

  void Abort();

  IMPL_EVENT_HANDLER(audiostart)
  IMPL_EVENT_HANDLER(soundstart)
  IMPL_EVENT_HANDLER(speechstart)
  IMPL_EVENT_HANDLER(speechend)
  IMPL_EVENT_HANDLER(soundend)
  IMPL_EVENT_HANDLER(audioend)
  IMPL_EVENT_HANDLER(result)
  IMPL_EVENT_HANDLER(nomatch)
  IMPL_EVENT_HANDLER(error)
  IMPL_EVENT_HANDLER(start)
  IMPL_EVENT_HANDLER(end)

  enum EventType {
    EVENT_START,
    EVENT_STOP,
    EVENT_ABORT,
    EVENT_AUDIO_DATA,
    EVENT_AUDIO_ERROR,
    EVENT_RECOGNITIONSERVICE_INTERMEDIATE_RESULT,
    EVENT_RECOGNITIONSERVICE_FINAL_RESULT,
    EVENT_RECOGNITIONSERVICE_ERROR
  };

  void DispatchError(EventType aErrorType, int aErrorCode, const nsAString& aMessage);
  void FeedAudioData(already_AddRefed<SharedBuffer> aSamples, uint32_t aDuration, MediaStreamListener* aProvider);

  friend class SpeechEvent;
private:
  enum FSMState {
    STATE_IDLE,
    STATE_STARTING,
    STATE_ESTIMATING,
    STATE_WAITING_FOR_SPEECH,
    STATE_RECOGNIZING,
    STATE_WAITING_FOR_RESULT,
  };

  class GetUserMediaStreamOptions : public nsIMediaStreamOptions
  {
  public:
    NS_DECL_ISUPPORTS
    NS_DECL_NSIMEDIASTREAMOPTIONS

    GetUserMediaStreamOptions() {}
    virtual ~GetUserMediaStreamOptions() {}
  };

  class GetUserMediaSuccessCallback : public nsIDOMGetUserMediaSuccessCallback
  {
  public:
    NS_DECL_ISUPPORTS
    NS_DECL_NSIDOMGETUSERMEDIASUCCESSCALLBACK

    GetUserMediaSuccessCallback(SpeechRecognition* aRecognition)
      : mRecognition(aRecognition)
    {}

    virtual ~GetUserMediaSuccessCallback() {}

  private:
    nsRefPtr<SpeechRecognition> mRecognition;
  };

  class GetUserMediaErrorCallback : public nsIDOMGetUserMediaErrorCallback
  {
  public:
    NS_DECL_ISUPPORTS
    NS_DECL_NSIDOMGETUSERMEDIAERRORCALLBACK

    GetUserMediaErrorCallback(SpeechRecognition* aRecognition)
      : mRecognition(aRecognition)
    {}

    virtual ~GetUserMediaErrorCallback() {}

  private:
    nsRefPtr<SpeechRecognition> mRecognition;
  };

  NS_IMETHOD StartRecording(DOMLocalMediaStream* aDOMStream);
  NS_IMETHOD StopRecording();

  uint32_t ProcessAudioSegment(AudioSegment* aSegment);
  void NotifyError(SpeechEvent* aEvent);

  void ProcessEvent(SpeechEvent* aEvent);
  FSMState TransitionAndGetNextState(SpeechEvent* aEvent);

  FSMState Reset();
  FSMState ResetAndEnd();
  FSMState StartedAudioCapture(SpeechEvent* aEvent);
  FSMState StopRecordingAndRecognize(SpeechEvent* aEvent);
  FSMState WaitForEstimation(SpeechEvent* aEvent);
  FSMState DetectSpeech(SpeechEvent* aEvent);
  FSMState WaitForSpeechEnd(SpeechEvent* aEvent);
  FSMState NotifyFinalResult(SpeechEvent* aEvent);
  FSMState DoNothing(SpeechEvent* aEvent);
  FSMState AbortSilently(SpeechEvent* aEvent);
  FSMState AbortError(SpeechEvent* aEvent);

  nsRefPtr<DOMLocalMediaStream> mDOMStream;
  nsCOMPtr<nsISpeechRecognitionService> mRecognitionService;

  void GetRecognitionServiceCID(nsACString& aResultCID);

  FSMState mCurrentState;
  bool mProcessingEvent;

  Endpointer mEndpointer;
  uint32_t mEstimationSamples;

  nsCOMPtr<nsITimer> mSpeechDetectionTimer;

};

class SpeechEvent : public nsRunnable
{
public:
  SpeechEvent(SpeechRecognition* aRecognition, SpeechRecognition::EventType aType)
  : mAudioSegment(0)
  , mRecognitionResultList(0)
  , mError(0)
  , mRecognition(aRecognition)
  , mType(aType)
  {
  }

  ~SpeechEvent();

  NS_IMETHOD Run();
  AudioSegment* mAudioSegment;
  nsRefPtr<SpeechRecognitionResultList> mRecognitionResultList; // TODO: make this a session being passed which also has index and stuff
  nsCOMPtr<nsIDOMSpeechRecognitionError> mError;

  friend class SpeechRecognition;
private:
  SpeechRecognition* mRecognition;

  // for AUDIO_DATA events, keep a reference to the provider
  // of the data (i.e., the SpeechStreamListener) to ensure it
  // is kept alive (and keeps SpeechRecognition alive) until this
  // event gets processed.
  nsRefPtr<MediaStreamListener> mProvider;
  SpeechRecognition::EventType mType;
};

} // namespace dom

inline nsISupports*
ToSupports(dom::SpeechRecognition* aRec)
{
  return static_cast<nsIObserver*>(aRec);
}
} // namespace mozilla
